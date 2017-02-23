-module(picturebot).

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

-record(state, {self}).
-define(STARTTEXT,"`picturebot: started`").
-define(HELPTEXT,"`do silly pictures`").

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
    slack_client:send_message(ChannelId, ?HELPTEXT),
    syn:join(slack_messages, self()),
    Self = slack_client:get_self(),
    {ok, #state{self = Self}}.

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

maybe_show_picture([Self,"wait","here","until" | M], ChannelId, _State, Self) ->
    Image = code:priv_dir(karlbot) ++ "/plugins/picturebot/wait_here.jpeg",
    Text = string:join(M, " "),
    FormatedText = prettypr:format(prettypr:text_par(Text), 16),
    {0, Picture} = my_exec("convert " ++ Image ++ " -pointsize 20 -annotate +430+380 '" ++ FormatedText ++ "' jpeg:-"),
    slack_client:send_file(ChannelId, Picture, "waithere.jpeg", "image/jpeg"),
    ok;

maybe_show_picture(_, _, _, _) ->
    ok.



process_command(Text, ChannelId, State) ->
    maybe_show_picture(Text, ChannelId, State, get_handle(State#state.self)).

get_handle(#{<<"id">> := Id}) ->
    "\<\@" ++ binary_to_list(Id) ++ "\>".

my_exec(Command) ->
    Port = open_port({spawn, Command}, [stream, in, eof, hide, exit_status]),
    get_data(Port, []).

get_data(Port, Sofar) ->
    receive
    {Port, {data, Bytes}} ->
        get_data(Port, [Sofar|Bytes]);
    {Port, eof} ->
        Port ! {self(), close},
        receive
        {Port, closed} ->
            true
        end,
        receive
        {'EXIT',  Port,  _} ->
            ok
        after 1 ->              % force context switch
            ok
        end,
        ExitCode =
            receive
            {Port, {exit_status, Code}} ->
                Code
        end,
        {ExitCode, lists:flatten(Sofar)}
    end.
