% Erlang Web Server
% 
% This erlang server handles connections between all components. It is the only
% thing that the front end communicates with. It combines the results received
% from both the database, and the twitter hashtag evaluator.
%
% Version 1.0: Intitial Release, accessing only from Evaluator, by Tobias Lindell
% Version 1.1: Combine data input from Database, by Kai Salmon and Tobias Lindell


-module(erlserver).
-export([start/1, keyvaluepairs_to_jsonpair/1, concat_jsonpair/1, add_JSON_to/2]).

% Returns northing.
%
% This method spawns a Bif which listens to a consistent socket.
% Port is a consistent port that the system is set up to. 8000 in our case. 
%
start(Port) ->
    spawn(fun () -> {ok, Sock} = gen_tcp:listen(Port, [{active, false}]), 
                    loop(Sock) end).


% Returns nothing.
%
% This method spawns a handler which sends a packet back through the connection
% after one is established.
%
% Sock is passed from the process spawned in start().
%
loop(Sock) ->
    {ok, Conn} = gen_tcp:accept(Sock),
    Handler = spawn(fun () -> handle(Conn) end),
    gen_tcp:controlling_process(Conn, Handler),
    loop(Sock).

% Returns nothing.
%
% This method passes a packet of data to the response method through the connection "Conn" established
% in the loop function.
%
handle(Conn) ->
	gen_tcp:send(Conn, response(do_recv(Conn))),
    gen_tcp:close(Conn).

% Returns nothing.
%
% This method changes the format of the packet so it can be used as a variable by the front end.
%
% The front end will receive a single string that is in the JSON format. This can be used effectively
% by javascript.
%
response(Str) ->	
    B = iolist_to_binary(Str),
    iolist_to_binary(
      io_lib:fwrite(
         "HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
         [size(B), B])).
		 

% Returns a string list with the combined results from passing the two front end tags into
% the Evaluator and the database.
%
% This method passes the two selected hashtags to the hashtag evaluator which returns a list
% of all the hash pairs. This list is then concatinated with the results of a database query
% using the same two tags.
%
% concat_jsonpair and add_JSON are both methods which change the format of hte lists to match
% JSON.
%
do_recv(Sock) ->
  S = connection:start(),
  {ok, Db} = connection:get_db(S,<<"tweetpairs">>),
  case gen_tcp:recv(Sock, 0) of
    {ok, Bin} ->

    	HaX = lindellnco_hashtag_evaluator:evaluate_hashtag(grab_x_from_string(Bin)),
		HaY = lindellnco_hashtag_evaluator:evaluate_hashtag(grab_y_from_string(Bin)),
   		
   		Xsearch = list_to_binary(grab_x_from_string(Bin)),

   		Str1 = grab_y_from_string(Bin),
   		[YList|_] = string:tokens(Str1,"\r"),
		Ysearch = list_to_binary(YList),


   		DbX = connection:get_hashtags(Db,Xsearch),
   		DbY = connection:get_hashtags(Db,Ysearch),

   		%o:format("~p~n------------------~n~p~n",[Xsearch, combine_lists(DbX++HaX)]),

   		X = remove_hashtag_from_list(remove_hashtag_from_list(combine_lists(DbX++HaX),Xsearch),Ysearch),
   		Y = remove_hashtag_from_list(remove_hashtag_from_list(combine_lists(DbY++HaY),Xsearch),Ysearch),

    	A = concat_jsonpair(X),
		B = concat_jsonpair(Y),
		add_JSON_to(A, B);


    {error, timeout} ->
      	io:format("timed_out");
    {error, Reason} ->
      	exit(Reason)
   end.

   	combine_lists([]) -> [];

	combine_lists([{H, F}|T]) ->
 		{Newtail, Fadd} = remove_from_list(T, T, H),
	 	[{H, (F+Fadd)}]++combine_lists(Newtail).

	remove_from_list([], OrigT, _H) ->
	 	{OrigT, 0};
	remove_from_list([{H, F}|_], OrigT, H) ->
	 	{lists:delete({H, F}, OrigT), F};
	remove_from_list([_|T], OrigT, H) ->
	 	remove_from_list(T, OrigT, H).

	remove_hashtag_from_list([],_Needle) -> [];


	remove_hashtag_from_list([{H,F}|T], Needle) -> 
		case to_lower(Needle) of
			H -> remove_hashtag_from_list(T,Needle);
			_ -> [{H,F}|remove_hashtag_from_list(T,Needle)]
		end.

% Returns a small string.
%
% This method extracts the required hashtag string from the jumbled string that gets
% received from the front end.
%
grab_x_from_string([Hi, Hj|T]) ->
	case [Hi, Hj] of
		[120, 61] -> record_x_string(T);
		_ -> grab_x_from_string([Hj, hd(T)|tl(T)]) end.

% Used by the grab_from_string method.
%
% This method starts recording the required string into a variable once the appropriate
% character has been found by grab_from_string.
%
record_x_string([H|T]) ->
	case H of
		38 -> [];
		_ -> [H] ++ record_x_string(T)
	end.
	
% Returns a small string.
%
% This method extracts the required hashtag string from the jumbled string that gets
% received from the front end.
%
grab_y_from_string([Hi, Hj|T]) ->
	case [Hi, Hj] of
		[121, 61] -> record_y_string(T);
		_ -> grab_y_from_string([Hj, hd(T)|tl(T)]) end.
		

% Used by the grab_from_string method.
%
% This method starts recording the required string into a variable once the appropriate
% character has been found by grab_from_string.
%
record_y_string([H|T]) ->
	case H of
		32 -> [];
		_ -> [H] ++ record_y_string(T)
	end.

% Returns a string in the JSON format.
%
% keyvaluepairs_to_jsonpair, concat_jsonpair, and add_JSON_to all manipulate the format of the
% raw data from the evaluator and database to match the JSON format.
%
keyvaluepairs_to_jsonpair({A, B}) ->
	binary_to_list(iolist_to_binary(io_lib:format("{\"tag\":\"~p\", \"freq\":\"~p\"}", [A, B]))).

concat_jsonpair([]) ->
	[];
concat_jsonpair([{A, B}]) ->
	keyvaluepairs_to_jsonpair({A, B});
concat_jsonpair([{A, B}|T]) ->
	keyvaluepairs_to_jsonpair({A, B})++","++concat_jsonpair(T).
	
	
add_JSON_to(L, Ll) ->
	 io_lib:format("{\"X\":[~p], \"Y\":[~p]}", [L, Ll]).	
		
% to_lower
%   Converts a bitsring to a list string, and converts each character to lower case.
to_lower(BitString) ->
  String = unicode:characters_to_list(BitString),
  string:to_lower(String).
