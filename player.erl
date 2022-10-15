%
%   这个 player 文件是专门给入门 gen_server 模式的新人观看的
%   主要表现为: 实现进程之间的异步通信与定时器使用
%
-module(player).
-behaviour(gen_server).

% API
-export([start/0, timer/0]).
-export([
    stop/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
]).

% -compile(export_all).
-record(player_data, {
    name = 0,
    num = 0
}).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    State = #player_data{name = jay, num = 1},
    erlang:put(data, State),
    {ok, State}.

handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab}.

% 其它消息的邮箱接收到了 my_timer 的消息
handle_info({my_timer, Msg}, State) ->
    % 打印一下它发了什么数据过来
    io:format("my_timer Data is ~p~n", [Msg]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%  player 端从信箱里拿到 Msg 为 get 的邮件
handle_cast(get, State) ->
    %  player端决定返回一个带数据的异步请求给server端
    Result = erlang:get(data),
    gen_server:cast(server, {hello, Result}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

% player决定3秒后给 player 发一条叫 my_timer 的消息并附带了一句 helloWorld
timer() ->
    erlang:send_after(3000, player, {my_timer, helloWorld}).

stop() -> gen_server:call(?MODULE, stop).
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
