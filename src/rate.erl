%%%-------------------------------------------------------------------
%% @doc Server to count, limit the rate per second basis.
%% @author ming
%% @end
%%%-------------------------------------------------------------------

-module(rate).

-behaviour(gen_server).

%% API
-export([add/0, add/1, remain/2,
         get_rates/0, print/0]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%
% bins for each rate counter
%%
-record(rates, {
    second              = 0,     %counts for the current second
    last3sec            = 0,     %counts for the last 3 seconds
    minute              = 0,     %counts for the last minute
    last5min            = 0,     %counts for the last 5 minutes
    last15min           = 0,     %counts for the last 15 minutes
    peak                = 0      %peak counts per second
}).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add(integer()) -> {ok, integer()}.
add(N) ->
    gen_server:call(?MODULE, {add, N}).

-spec add() -> {ok, integer()}.
add() -> add(1).

%%
% Return the remaining token or error if the limit is reached.
%
%%
-spec remain(integer(), pos_integer()) -> {ok, integer()} | {error, neg_integer()}.
remain(N, Limit) ->
    case add(N) of
        {ok, CurrentRate} when Limit >= CurrentRate ->
            {ok, Limit - CurrentRate};
        {ok, CurrentRate} when Limit < CurrentRate ->
            {error, Limit - CurrentRate};
        _ ->
            {error, -1234567890} %sepecial case where the add fails
    end.

-spec get_rates() -> any().
get_rates() ->
    gen_server:call(?MODULE, get).

%%
% pretty print rates.
%%
print() ->
    Stats = get_rates(),
    Fields = record_info(fields, rates),
    io:format(io_lib_pretty:print(Stats,
      fun(rates, _Size) -> Fields end )
    ).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
    Timer = erlang:send_after(1000, self(), reset),
    {ok, {#rates{}, Timer, [], [], [], []}}.
                        %{[3 second list], [minute list], [5 minute list], [15 minute list]}

%%
%%
handle_call({add, N}, _From, State) ->
    {Rate, Timer, L3S, L1, L5, L15} = State,
    NewRate = Rate#rates{second = Rate#rates.second + N},
    {reply, {ok, NewRate#rates.second}, {NewRate, Timer, L3S, L1, L5, L15}};

handle_call(get, _From, State) ->
    {Rate, _Timer, _, _, _, _} = State,
    {reply, Rate, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reset, State) ->
    {Rate, OldTimer, L3S, L1, L5, L15} = State,
    erlang:cancel_timer(OldTimer),
    CurrentRate = Rate#rates.second,
    Timer = erlang:send_after(1000, self(), reset),
    {NewL3S, NewS3S} = count(CurrentRate, L3S, Rate#rates.last3sec, 3),
    {NewL1, NewS1} = count(CurrentRate, L1, Rate#rates.minute, 60),
    {NewL5, NewS5} = count(CurrentRate, L5, Rate#rates.last5min, 5*60),
    {NewL15, NewS15} = count(CurrentRate, L15, Rate#rates.last15min, 15*60),
    NewRate = Rate#rates{second = 0, last3sec = NewS3S, minute = NewS1,
                       last5min = NewS5, last15min = NewS15,
                       peak = erlang:max(Rate#rates.peak, CurrentRate)},

    {noreply, {NewRate, Timer, NewL3S, NewL1, NewL5, NewL15}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
count(N, BucketList, Sum, UpperLimit) when length(BucketList) >= UpperLimit ->
    {H, T} = lists:split(1, BucketList),
    [Head] = H,
    NewSum = Sum - Head + N,
    NewBucketList = lists:append(T, [N]),
    {NewBucketList, NewSum};
count(N, BucketList, Sum, _UpperLimit) ->
    NewSum = Sum + N,
    NewBucketList = lists:append(BucketList, [N]),
    {NewBucketList, NewSum}.
