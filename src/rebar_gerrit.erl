%%%-----------------------------------------------------------------------------
%%% @copyright 2020
%%% @doc {@module} This plugin allows the rebar3 to download a gerrit reference
%%%
%%% @author Thiago Esteves <thiago.calori@gmail.com>
%%% @end
%%%-----------------------------------------------------------------------------

-module(rebar_gerrit).

-behaviour(rebar_resource_v2).

%%%===================================================================
%%% INTERFACE API
%%%===================================================================

-export([init/2,
         lock/2,
         download/4,
         needs_update/2,
         make_vsn/2]).

%% For backward compatibilty
-export([ download/3 ]).

%%%===================================================================
%%% API IMPLEMENTATION
%%%===================================================================

-spec init(atom(), rebar_state:t()) -> {ok, rebar_resource_v2:resource()}.
init(Type, _State) ->
    Resource = rebar_resource_v2:new(Type, ?MODULE, #{}),
    {ok, Resource}.

lock(AppInfo, _) ->
    lock_(rebar_app_info:dir(AppInfo), rebar_app_info:source(AppInfo)).

lock_(_AppDir, {gerrit, Url, {ref, Ref}}) ->
    {gerrit, Url, {ref, Ref}}.

needs_update(AppInfo, _) ->
  needs_update_(rebar_app_info:dir(AppInfo), rebar_app_info:source(AppInfo)).

needs_update_(_Dir, {gerrit, _Url, {ref, _Ref}}) ->
  %% TODO: Need to be implemented
  false.

download(TmpDir, AppInfo, State, _) ->
    case download_(TmpDir, rebar_app_info:source(AppInfo), State) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, Reason};
        Error ->
            {error, Error}
    end.

%% For backward compatibilty
download(Dir, AppInfo, State) ->
    download_(Dir, AppInfo, State).

download_(Dir, {gerrit, Url, {ref, Refs}}, _State) ->
    ok = filelib:ensure_dir(Dir),
    AbortMsg = io_lib:format("Get revision ~ts from gerrit failed", [Refs]),
    %% Download Master Branch
    {ok, _} = rebar_utils:sh(lists:flatten(
                     io_lib:format("git clone ~ts ~ts",
                                   [rebar_utils:escape_chars(Url),
                                    rebar_utils:escape_chars(filename:basename(Dir))])),
                     [{cd, filename:dirname(Dir)}, {use_stdout, false}]),
    %% Fetch to the respective reference at gerrit
    rebar_utils:sh("git -C " ++ rebar_utils:escape_double_quotes(Dir) ++
                   " fetch origin " ++ rebar_utils:escape_double_quotes(Refs),
                     [{use_stdout, false}, {debug_abort_on_error, AbortMsg}]),
    rebar_utils:sh("git -C " ++ rebar_utils:escape_double_quotes(Dir) ++
                   " checkout FETCH_HEAD",
                     [{use_stdout, false}, {debug_abort_on_error, AbortMsg}]).

make_vsn(_, _) ->
    {plain, "1.0.0"}.
