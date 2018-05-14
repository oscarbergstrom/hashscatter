% CouchDb connector/manipulator
%
%
% NOTE: This erlang file makes a connection to a CouchDB database as well as manipulating its data
% 
% .


-module(connection).

-export([start/0, get_db/2, test/0, make_doc/2, get_freq/3, get_value/3, get_hashtags/2, write_hashtags_to_db/2]).

% start():
%   starts few applications needed to start couchbeam API 
%   makes a connection to the database on the server 
%   using couchbeam API.

start() ->
	application:start(mimerl),
	application:start(certifi),
	hackney:start(),
	application:start(couchbeam),
	Url = "hal2000.skip.chalmers.se:1983",
	Options = [],
	S = couchbeam:server_connection(Url, Options),
	S.

% create_db(S,Name):
%   Creates a new database on the server with the given name
%   S: the connector to the server (hal2000.skip.chalmers.se:1983) 
%   Name: name of the database created in the server 
create_db(S,Name) -> 
	
	Options = [],
	
	Result = couchbeam:create_db(S, Name, Options),
	
	case Result of 
		{ok, Db} ->
			{ok, Db};
		{error,Error} -> 			
			{error,Error};
		Error ->
			{unkown_response,Error}
	end.
	
% open_db(S,Name):
%   Opens a connection to an individual Database on the given Server
%   S: the connector to the server (hal2000.skip.chalmers.se:1983) 
%   Name: name of the database created in the server 

open_db(S,Name)-> 
	Options = [],
	{ok, Db} = couchbeam:open_db(S, Name, Options),
	{ok,Db}.

% get_db(S, Name):
%	Attempts to create a new database with the given name. If this fails, because
%   because a database with that name already it instead returns a connection to that
%   database.
%   S: the connector to the server (hal2000.skip.chalmers.se:1983) 
%   Name: name of the database created in the server 

get_db(S,Name)->
	
	Response = create_db(S,Name),
	
	case Response of
		{ok, Db} -> 
			
			{ok,Db};
		{error,db_exists} -> 
			
			{ok, Db} = open_db(S, Name),
			{ok,Db};
		{error,Error} -> 
			
			
			{error,Error}
	end.

%make_doc
%	Adds a new document to a DB on the server
%	Db: The database Connection
%   Doc: A document to be added
make_doc(Db, Doc) -> 
	%io:format("DOC: ~p~n",[Doc]),
	%io:format("~p~n", [
		couchbeam:save_doc(Db, Doc)
	%])
	.

%get_doc
%	Returns a document with a given ID from the DB
%	Db: The database Connection
%   ID: The ID of the document
get_doc(Db, Id) ->
	R = couchbeam:open_doc(Db, Id), R.
	%io:format("R: ~p~n",[R]).

%get_freq
%	Fetches the frequency of a given pair of hashtags in the database.
% 	IF no record is found, it instead returns zero
%	Db: The database Connection
%   Xl: The X Hashtag, as a bitstring
%   Yl: The Y Hashtag, as a bitstring
get_freq(Db, Xl, Yl) ->
	X = list_to_binary(Xl), Y = list_to_binary(Yl),  
	%io:format("~p  &  ~p ~n",[X,Y]),
	DesignName = {<<"pair_to_freq">>, <<"pair_to_freq">>},
	{ok, ViewResults} = couchbeam_view:fetch(Db, DesignName, [{key,[X,Y]}]),
	%io:format("ViewResults:~p~n ",[ViewResults]),
	Freq = get_value([X,Y],ViewResults,<<"0">>),
	case Freq of
		{_,<<"0">>} -> ok; %io:format("!");
		_ -> ok %io:format("Freq:~p~n ",[Freq])
	end,
	Freq.

%get_value
%	Returns the value and ID of the item with a give key in a list of JSON style id-key-value triple,
% 	When no Key is found a default value is returned, and the ID -1 is used
%	Key; The key to be searhged for
%	List: The list of id-key-value triples
%	Def: The default value to be returned
get_value(_,[],Def) -> {<<"-1">>, Def};
get_value(Key,[
	{[
		{<<"id">>,ID},
	    {<<"key">>,Key},
	    {<<"value">>,Val}
     ]}
     |_],_Def)->{ID, Val};
get_value(Key,[_|T],Def) ->get_value(Key,T,Def).

	
%test
%	A method used to test the start, get_db and make_doc methods
test() -> 
	
	S = start(),
	
	{ok, Db} = get_db(S,"testymctest4"),
	
	%io:format(":::~n~p~n",[Db]),
	Doc = {[
		{<<"_id">>, <<"test">>},
		{<<"content">>, <<"some  changed text">>}
	]},
	
	make_doc(Db,Doc),
	
	ok.

%get_hashtags
%	Returns a list of Hashtag-Frequency pairs where the given Hashtag corrisponds with 
%	each hashtag in the database
%	Db: The Database connection
%	Hash: The Hashtag being searched for
get_hashtags(Db, Hash) ->
	DesignName = {<<"hash_to_hash">>, <<"hash_to_hash">>},
	{ok, ViewResults} = couchbeam_view:fetch(Db, DesignName, [{key,Hash}]),
	[{binary_to_list(H),element(1,string:to_integer(binary_to_list(F)))} ||  {[_,_,{_,{[{_,H},{_,F}]}}]} <- ViewResults].

%write_hashtags_to_db:
%	Writes a list of hashtag-hashtag pairs to the database, incrementing that pairs frequency by one.
% 	If no pair is found in the database, it is added with a frequency of one.
%	PairDB: The Database of hashtag pairs
%	HashtagList: List hashtag-hashtag pairs. For each pair, a reversed version should be in the list.
write_hashtags_to_db(_PairDB, []) -> ok;
write_hashtags_to_db(PairDB, [{X, Y}|T]) ->
  % write H to database
  io:format("♫"),
  try 
    {ID,F} = connection:get_freq(PairDB, X, Y),
    FPlus = integer_to_binary(binary_to_integer(F)+1),
    case ID of 
      <<"-1">> -> ok;
      _ ->
        {ok, Doc} = couchbeam:open_doc(PairDB,ID),
        couchbeam:delete_doc(PairDB,Doc)

    end,
    Doc_hashcombos = {[
      {<<"x">>, list_to_binary(string:to_lower(unicode:characters_to_list(X)))},
      {<<"y">>, list_to_binary(string:to_lower(unicode:characters_to_list(Y)))},
      {<<"freq">>, FPlus}
    ]}, 
    make_doc(PairDB, Doc_hashcombos),
    write_hashtags_to_db(PairDB, T)
  catch
    _:R -> io:format("~nR: ~p~n",[R]),
    write_hashtags_to_db(PairDB, T)
  after
    ok%do anything which has to be done regardless of crash
  end.
