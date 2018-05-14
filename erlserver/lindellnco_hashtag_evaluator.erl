% Twitter Hashtag evaluator
%
% By Kai Salmon, Using Kivra's Oauth2 Client (and dependencies)
%
% NOTE: This erlang file make heavy uses of list based stings and bitstrings-
% are two different ways erlang can represent strings.
% BITSTRINGS: Are written as <<"some string">> and are stored as binary. They cannot be modified.
% LIST STRINGS: Are written as "some string", and are actually "syntatical sugar" for a list of numbers, 
%               which happen to represent characters.

-module(lindellnco_hashtag_evaluator).

-export([evaluate_hashtag/1,run/0, get_last_tweets/2,get_hashtag_tweets/2, hashtag_url/2,append_binary/1
        ]).

-define(CONSUMER_SECRET, <<"cWB52kDqZwXvjEEIJanUHFZTGUDOeQwWDLE4xYuckCE2ZMzjO2">>).
-define(CONSUMER_KEY, <<"lO4Vylj6MS33VYmLBEqRxjquJ">>).

-define(OAUTH2_TOKEN_URL, <<"https://api.twitter.com/oauth2/token">>).

-define(USER_TIMELINE_URL(User, StrCount),
        <<"https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="
          , User, "&count=", StrCount>>).

-define(APP_LIMITS_URL(Resources),
        << "https://api.twitter.com/1.1/application/rate_limit_status.json?resources="
           , Resources>>).

% run():
%   Example given by Kivra (author of the Oauth2 Client library)
%   Sends two GET requests to api.twitter, one requesting tweets from the user
%   "twitterapi"'s account, and one requesting infomation relating to the Rate Limitations
%   given by twitter  
run() ->
    inets:start(),
    ssl:start(),
    {ok, _Headers, Client} =
        oauth2c:retrieve_access_token(
          <<"client_credentials">>, ?OAUTH2_TOKEN_URL, ?CONSUMER_KEY,
          ?CONSUMER_SECRET),
    {{ok, _Status1, _Headers1, Tweets}, Client2} =
        oauth2c:request(
          get, json, ?USER_TIMELINE_URL("twitterapi", "4"), [200], Client),
    io:format("Tweets: ~p~n", [Tweets]),
    {{ok, _Status2, _Headers2, Limits}, _Client3} =
        oauth2c:request(
          get, json, ?APP_LIMITS_URL("help,users,search,statuses"),
          [200], Client2),
    io:format("Limits: ~p~n", [Limits]),
    ok.

% get_last_tweets(From, N): 
%   Returns a list containing the text of the last tweets from a given user 
%   FROM: The user, as a bit string, who's tweets will be requested
%   N: The number of tweets to fetch, also a bitstring
get_last_tweets(From,N) ->
    inets:start(),
    ssl:start(),
    {ok, _Headers, Client} =
        oauth2c:retrieve_access_token(
          <<"client_credentials">>, ?OAUTH2_TOKEN_URL, ?CONSUMER_KEY,
          ?CONSUMER_SECRET),
    {{ok, _Status1, _Headers1, Tweets}, _Client2} =
        oauth2c:request(
          get, json, user_timeline_url(From, N), [200], Client),
    io:format("Tweets: ~p~n", [get_text_from_tweets(Tweets)]),
    ok.

% get_hashtag_tweets(Hashtag, N): 
%   Fetches a number of the last tweets containing the given hashtag, and returns 
%   a list containing a lists of hashtags contained in each tweet.
%   Returns:
%     {Hashtag list list, max id in tweets}
%   HASHTAG: The hashtag, as a bit string, who's tweets will be requested
%   N: The number of tweets to fetch, also a bitstring
get_hashtag_tweets(Hash,N) ->
    inets:start(),
    ssl:start(),
    {ok, _Headers, Client} =
        oauth2c:retrieve_access_token(
          <<"client_credentials">>, ?OAUTH2_TOKEN_URL, ?CONSUMER_KEY,
          ?CONSUMER_SECRET),
    OauthResponse =
        oauth2c:request(get, json, hashtag_url(Hash, N), [200], Client),
    case OauthResponse of
        {{ok, _Status1, _Headers1, Response}, _Client2} -> 
            [{<<"statuses">>,Tweets},_Other] = Response,
            {get_hashtags_from_tweets(Tweets), min_id(Tweets)};
        X -> 
            io:format("Unexpected oAuthResponse:~p~n",[X]),
            []
    end.
get_hashtag_tweets_since(Hash,N,Id) ->
    inets:start(),
    ssl:start(),
    {ok, _Headers, Client} =
        oauth2c:retrieve_access_token(
          <<"client_credentials">>, ?OAUTH2_TOKEN_URL, ?CONSUMER_KEY,
          ?CONSUMER_SECRET),
    {{ok, _Status1, _Headers1, Response}, _Client2} =
        oauth2c:request(
          get, json, hashtag_url_since(Hash, N,Id), [200], Client),
    [{<<"statuses">>,Tweets},_Other] = Response,
    %io:format("Tweets: ~p~n", [get_text_from_tweets(Tweets)]),
    {get_hashtags_from_tweets(Tweets), min_id(Tweets)}.

% user_timeline_url(User, StrCount): 
%   Returns a bitstring containing the URL which will make a request for a users timeline.
%   USER: The User, as a bit string, who's tweets will be requested
%   StrCount: The number of tweets to fetch, also a bitstring
user_timeline_url(User, StrCount) -> 
  append_binary(
      [
        <<"https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=">>,
        User, 
        <<"&count=">>,
        StrCount
      ]
    ).

% get_hashtag_tweets(Hashtag, N): 
%   Returns a bitstring containing the URL which will make a request for the N most recent tweets containing the 
%   given tweet.
%   HASHTAG: The hashtag, as a bit string, who's tweets will be requested
%   N: The number of tweets to fetch, also a bitstring
hashtag_url(Hash, StrCount) ->
  append_binary([<<"https://api.twitter.com/1.1/search/tweets.json?q=%23">>,Hash,<<"&count=">>,StrCount,<<"&result_type=recent">>]).
hashtag_url_since(Hash, StrCount,Id) ->
  IdString = list_to_binary(integer_to_list(Id)),
  S = append_binary([<<"https://api.twitter.com/1.1/search/tweets.json?q=%23">>,Hash,<<"&count=">>,StrCount,<<"&result_type=recent&max_id=">>,IdString]),
  S.

% append_binary(A,B)
%   Util function to append two bitstrings.
%   (It does this by converting them into lists, and back again)
append_binary(A,B) -> 
  erlang:list_to_binary(erlang:binary_to_list(A)++erlang:binary_to_list(B)).

% append_binary(A,B)
%   Util function to append a list of bitstrings.
append_binary([]) -> <<"">>;
append_binary([H|[]]) -> H;
append_binary([H|T]) -> append_binary(H,append_binary(T)).

% get_text_from_tweet(Tweet)
%   returns the text of a tweet
%   TWEET: A list, representing a single Tweet, in the format as given by twitter
get_text_from_tweet(Tweet) -> get_field(<<"text">>,Tweet).


% get_text_from_tweets(TweetList)
%   returns a list containing the text from all the tweets in a list
%   TWEETLIST: A list of tweets, in the format as given by twitter
get_text_from_tweets(TweetList) -> [get_text_from_tweet(T)|| T <- TweetList].

% get_hashtags_from_tweets(TweetList)
%   Returns a list containing a lists of hashtags contained in each tweet in the TweetList
%   TWEETLIST: A list of tweets, in the format as given by twitter
get_hashtags_from_tweets(TweetList) -> [extract_hashtags(T)|| T <- TweetList].


% get_field(Field,Object)
%   Returns the value of field for a given object.
%   FIELD: The field being fetched
%   OBJECT: A list of key/value pairs, representin the object.
get_field(_,[]) -> undef;
get_field(Field,[{Field,Value}|_]) -> Value;
get_field(Field,[_|T]) -> get_field(Field,T);
get_field(_,_) -> invalid_tweet.

% extract_hashtags(T)
%   returns a list of hashtags contained within a tweet.
%   TWEET: A list, representing a single Tweet, in the format as given by twitter  
extract_hashtags(T) -> 
E = get_field(<<"entities">>,T),
H = get_field(<<"hashtags">>,E),
[to_lower(X)||[{<<"text">>,X},_]<-H].

% reducer
%   A process which can recieve hashtag messages from
%   any number of parallel processes ,
%   and store how many instancies of each atom it
%   recieves. 
%   The process finishes when it receieves a finish message
%   at which point it sends the list to the sender of the finish
%   message.
%   LIST: A list of Key/Value pairs (a list of tuples representing the hashtag and their frequency) e.g.
%       [{"apple",1},{"dog",25},{"vampire",5}]
reducer(List) -> 
  receive 
    {hashtag,Hashtag} ->
      case get_field(Hashtag,List) of
        undef -> reducer([{Hashtag,1}|List]);
        Value -> reducer(set_value(Hashtag,Value+1,List))
      end;
    {finish,From} ->
      From ! List
  end.

% set_value(Key,Value,List)
%   returns a modifed version of the Key/Value list where the value of the given 
%   Key is replaced with the given value
set_value(_Key,_Value,[]) -> [];
set_value(Key,Value,[{Key,_OldValue}|T]) -> [{Key,Value}|T];
set_value(Key,Value,[H|T]) -> [H|set_value(Key,Value,T)].

% count_hashtags_from_tweet_list(TweetList)
%   Given a list of lists-of-hashtags,
%   returns a Key/Value list containing each hashtag
%   and their frequency
count_hashtags_from_tweet_list(TweetList) ->
  X = spawn(fun()->reducer([]) end),
  pforeach(fun(List)->
    pforeach(fun(Item)->
      X ! {hashtag,Item}
    end,List)
  end,TweetList),
  X ! {finish,self()},
  receive
    [H|T] -> [H|T]
    after 7000 -> error
  end.

% pforeach(Fun,List)
%   Spawns a process to run the given function on each element of the list, 
%   and then waits for each process to finish. Returns ok, or a time_out if 
%   the process takes over 5 seconds
%   FUN: A function which takes one argument
%   LIST: The list of elements
pforeach(_,[]) -> ok;
pforeach(Fun,List) ->
  This = self(),
  N = length(List),
  lists:foreach(fun(X)->
    spawn(fun()->
      Fun(X),
      This ! done
    end)
  end,List),
  wait_for_N_dones(N).

% wait_for_N_Dones(N)
%   waits until the process receieves the atom done N times
wait_for_N_dones(0)->ok;
wait_for_N_dones(N)->
  receive
    done -> wait_for_N_dones(N-1)
    after 5000 -> time_out
  end.

% evaluate_hashtag(Hashtag)
% returns a Key/Value pair list containing the frequency of other 
% hashtags appearing along side the given hashtag, taken from a sample
% of the 100 most recent tweets.
% HASHTAG: The hashtag, as either a list string or a bitstring, to be evaluated
evaluate_hashtag(String) when erlang:is_list(String) -> evaluate_hashtag(binary:list_to_bin(String));
evaluate_hashtag(BitString) -> 
  io:format("Searching for:\"~p\"~n",[BitString]),
  {FirstTweetList,MaxId} = get_hashtag_tweets(BitString,<<"100">>),
  MultipleTweetList= repeat_get_hashtag_tweets(BitString,2,MaxId),
  Hashtags = count_hashtags_from_tweet_list(
    FirstTweetList++MultipleTweetList
  ),
  List = lists:sort(fun({_,A},{_,B})-> A>B end,Hashtags),
  io:format("~p results returned for search\"~p\"~n",[length(List),BitString]),
  List.

repeat_get_hashtag_tweets(_,0,_) -> [];
repeat_get_hashtag_tweets(BitString,N,MaxId) ->
  {Tweets, NewMax} = get_hashtag_tweets_since(BitString,<<"100">>,MaxId),
  Tweets ++ repeat_get_hashtag_tweets(BitString,N-1,NewMax).

% to_lower
%   Converts a bitsring to a list string, and converts each character to lower case.
to_lower(BitString) ->
  String = unicode:characters_to_list(BitString),

  string:to_lower(String).

%returns the highest id from a list of tweets
min_id([]) -> infinity;
min_id([Tweet|T]) ->  
    X = get_field(<<"id">>,Tweet),
    min(X,min_id(T)).