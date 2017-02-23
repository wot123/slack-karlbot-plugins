-module(random_image).

-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).

%% API functions
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {self, pid}).
-define(STARTTEXT,"`random image: started`").

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ChannelId) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ChannelId], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([ChannelId]) ->
    slack_client:send_message(ChannelId, ?STARTTEXT),
    syn:join(slack_messages, self()),
    Self = slack_client:get_self(),
    {ok, Pid} = gun:open("www.google.co.uk", 80),
    {ok, #state{self = Self, pid = Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info([{<<"type">>,<<"message">>},
             {<<"channel">>, ChannelId},
             {<<"user">>, _UserId},
             {<<"text">>, Text},_,_], State) ->
    process_command(string:tokens(binary_to_list(Text)," "), ChannelId, State),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_show_picture(50, M, ChannelId, State, _Self) ->
    Text = string:join(M, "+"),
    URL = "/search?tbs=itp:animated&tbm=isch&q=" ++ Text ++ "&safe=active",
    StreamRef = gun:get(State#state.pid, URL, [{<<"host">>,"www.google.co.uk"},
                                               {<<"user-agent">>, "Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_0 like Mac OS X; en-us) AppleWebKit/532.9 (KHTML, like Gecko) Versio  n/4.0.5 Mobile/8A293 Safari/6531.22.7"}]),
    {ok, Response} = gun:await_body(State#state.pid, StreamRef),
    Body = binary:replace(Response, <<"\\x">>, <<"%">>, [{insert_replaced, 1}]),

    {match, L} = re:run(Body, "var u='(.*?)'", [global, {capture, [1], binary}]),
    RandomList = [X||{_,X} <- lists:sort([{rand:uniform(), N} || N <- L])],
    [[Image]|_] = RandomList,
    slack_client:send_message(ChannelId, binary_to_list(Image)),
    ok;

maybe_show_picture(_, _, _, _, _) ->
    ok.



process_command(Text, ChannelId, State) ->
    maybe_show_picture(rand:uniform(51), Text, ChannelId, State, get_handle(State#state.self)).

get_handle(#{<<"id">> := Id}) ->
    "\<\@" ++ binary_to_list(Id) ++ "\>".
