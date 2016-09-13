-module(spg_router).

-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).

-export([route/2]).

-export([route/1]).
-export([design/1]).
-export([view/1]).
-export([sys/1]).

-define(desing, "_design").
%-define(?NI(What, Method, Path, State),
%        lager:warning(iolist_to_binary(["NI", Method, " ", Path

-define(is_get(X), X =:= <<"GET">> orelse X =:= <<"HEAD">>).

init(_Type, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
  {ok, Req, [Opts]}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"PATCH">>, <<"PUT">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{'*', route}], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"text/plain">>, route}, {<<"application/json">>, route}], Req, State}.

route(Req, State) ->
  [Path, Method] = cowboy_req:get([path, method], Req),
  {QVL, Req1} = cowboy_req:qs_vals(Req),
  lager:info(State, "handling ~ts ~ts with QSVals ~p", [Method, Path, QVL]),
  QV = maps:from_list(QVL),
  Tokens = [cow_qs:urldecode(X) || X <- re:split(Path, "/"), X /= <<>>],
  lager:debug(State, "tokens ~p", [Tokens]),
  {FunName, TokenArgs} = case Tokens of
                           [<<$_, _/binary>> | _] = P ->
                             {sys, [P]};
                           [Db, <<"_design">>, Name, <<"_view">> | P] ->
                             {view, [Db, <<"_design/", Name/binary>>, P]};
                           [Db, <<"_design">>, Name | P] ->
                             {design, [Db, [<<"_design/", Name/binary>> | P]]};
                           [] ->
                             {route, [[]]};
                           _ ->
                             {route, [Tokens]}
                         end,
  {ArgsTail, Req2} = case ?is_get(Method) of
                       true ->
                         {[State], Req1};
                       false ->
                         {ok, Doc, Req_} = cowboy_req:body(Req1),
                         {[Doc, State], Req_}
                     end,
  Args = [Method, QV | TokenArgs] ++ ArgsTail,

  {Code, Headers, Body, State1} = case apply(?MODULE, FunName, [Args]) of
                                    {Code_, Body_}              -> {Code_, [], Body_, State};
                                    {Code_, Headers_, Body_}    -> {Code_, Headers_, Body_, State}
                                  end,
  Code1 = case {Code, Method} of
            {true, <<"PUT">>} -> 201;
            {true, _} -> 200;
            {false, _} -> 404;
            _ -> Code
          end,
  Body1 = case Body of
           'NI' -> <<"{\"error\":\"not_found\",\"reason\":\"Not implemented.\"}">>;
           _    -> Body
         end,
  lager:debug(State, "setting code ~p", [Code1]),
  lager:debug(State, "body ~p", [Body1]),
  {ok, Req3} = cowboy_req:reply(Code1, Headers, Body1, Req2),
  Code1 =:= 404 andalso not(?is_get(Method))
            andalso lager:warning(State, "404 on ~ts ~ts: ~p", [Method, Path, Body1]),
  {halt, Req3, State1}.

route([<<"GET">>, _, [], _State]) ->
  {true, <<"{\"couchdb\":\"Welcome\",\"version\":\"1.1.1\",\"bigcouch\":\"0.4.2\"}">>};

route([Method, _Params, [Db], State])
    when ?is_get(Method) ->
  dbquery("select * from db($1)", [Db], State);
route([<<"PUT">>, _QS, [Db], State]) ->
  dbquery("select * from db_create($1)", [Db], State);

route([Method, QS, [Db, <<"_all_docs">>], State])
    when ?is_get(Method) ->
  dbquery("select * from all_docs($1, $2)", [Db, QS], State);
route([Method, _QS, [Db, <<"_changes">>], State])
    when ?is_get(Method) ->
  dbquery("select * from changes($1)", [Db], State);
route([Method, _QS, [_Db, <<"_", _/binary>> = Query | _], State]) ->
  lager:warning([{not_implemented, true} | State], "NI ~s db spec ~s", [Method, Query]),
  {404, 'NI'};
route([Method, _QS, [Db, DocId], State])
    when ?is_get(Method) ->
  dbquery("select * from doc($1, $2)", [Db, DocId], State);

route([Method, _QS, Path, State]) ->
  lager:warning([{not_implemented, true} | State], "NI ~s: ~p", [Method, Path]),
  {501, 'NI'};

route([<<"PUT">>, _QS, [Db], _DbName, State]) ->
  dbquery("select * from db_create($1)", [Db], State);
route([<<"PUT">>, _QS, [Db, DocId], Doc, State]) ->
  dbquery("select * from doc_create($1, $2, $3)", [Db, DocId, Doc], State);
route([Method, _QS, Path, _Doc, State]) ->
  lager:warning([{not_implemented, true} | State], "NI ~s: ~p", [Method, Path]),
  {501, 'NI'}.

dbquery(Query, Params, State) ->
  lager:debug([{pg, true} | State], "postgresql query ~s", [Query]),
  case spgdb:equery(Query, Params) of
    {ok, _, [{_, null}]} ->
      {false, <<>>};
    {ok, _Columns, [{_Executed, _Info} = Answer]} ->
      lager:debug(State, "executed ~p", [_Executed]),
      lager:debug(State, "info ~p", [_Info]),
      handle_db_result(Answer);
    {error, {error, Status, ErrNum, ErrMsg, ErrDetail}} ->
      lager:alert([{pg, true} | State], "psql error: ~p", [{Status, ErrNum, ErrMsg, ErrDetail}]),
      {500,
       <<
         "{"
         "\"PgErrNum\":\"", ErrNum/binary
         ,"\",\"PgMsg\":\"", ErrMsg/binary
%         ,"\",\"PgDetail\":\"", ErrDetail/binary, "\""
         ,"}"
       >>
      }
  end.

handle_db_result({true, _} = Answer) ->
  Answer;
handle_db_result({false, InfoBin} = Answer) ->
  Info = jsxn:decode(InfoBin),
  case Info of
    #{<<"error">> := <<"file_exists">>} ->
      {412, InfoBin};
    _ ->
      Answer
  end.

sys([Method, _QS, [<<"_all_dbs">>], State])
    when ?is_get(Method) ->
  dbquery("select * from all_dbs()", [], State);
sys([Method, _QS, [<<"_active_tasks">>], State])
    when ?is_get(Method) ->
  {true, <<"[]">>};
sys([Method, _QS, [<<"_log">>], State])
    when ?is_get(Method) ->
  {true, <<"">>};
sys([Method, _QS, Path, State]) ->
  lager:warning([{not_implemented, true} | State], "NI SYSTEM VIEW: ~s: ~p", [Method, Path]),
  {404, 'NI'}.

design([Method, _QS, Db, [DocId], State]) when ?is_get(Method) ->
  dbquery("select * from db_design($1, $2)", [Db, DocId], State);
design([Method, _QS, Db, Path, State]) ->
  lager:warning([{not_implemented, true} | State], "NI DESIGN: ~s ~s: ~p", [Method, Db, Path]),
  {404, 'NI'};

design([<<"PUT">>, _QS, Db, [DocId], Doc, State]) ->
  dbquery("select * from db_design_create($1, $2, $3)", [Db, DocId, Doc], State).

view([Method, _QS, Db, Design, [View], State]) when ?is_get(Method) ->
  dbquery("select * from db_view($1, $2, $3)", [Db, Design, View], State);
view([Method, _QS, Db, Name, Path, State]) ->
  lager:warning([{not_implemented, true} | State], "NI VIEW: ~s ~s ~s ~p", [Method, Db, Name, Path]),
  {404, 'NI'}.
