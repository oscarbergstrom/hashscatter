Information is not up to date and the project is not online. /2018-05-14

The website is currently active on: http://hal2000.skip.chalmers.se/

Setup Guide:

1. This project requires a working PHP server. Set one up.

2. Set siteroot as the root directory for the website under the domain "http://hal2000.skip.chalmers.se".

3. Download and install couchDB onto the server. In the config file,
set the port to 1983.

4. Start the erlang process erlserver/erlserver:start(8000). This starts the erlang web server
on the port 8000.

5. Start the twitterminer process erlserver/twitterminer:run().


This folder contains files that we have created for the Hashscatter system project. 
There are many files however that are required for the system to run, but ones that we did not create.

The files we created include:
- index.html
- EvaluateHashtag.php
- Style/Main.css
- Scripts/graph.js
- erlserver/erlserver.erl
- erlserver/connection.erl
- erlserver/lindellnco_hashtag_evaluator.erl
- erlserver/twitterminer.erl


index.html -

This is the html file that builds the structure of our webpage.


EvaluateHashtag.php -

This php file allows for our server to open a specific port, which enables
the connection between client and server.


Main.css -

This css file contains the design of our webpage. It also allows for the responsive
design which makes our webpage usable on screens of all sizes.


graph.js -

This javascript file contains the logic which manipulates the output from Twitter and
turns it into usable data in the form of a scatter graph.


erlserver.erl -

This erlang module starts up a web server. This server handles almost all the connections between
components in the system. It communicates with the client, as well as the other server-side components.


connection.erl -

This erlang module uses a couchDB API to make the necessary interactions with the database, like
inserting and retrieving data.


lindellnco_hashtag_evaluator.erl -

This erlang module uses the Rest API to connect to Twitter. It can get a list of all tags used with a given
up to the most recent 500 tweets. (depending on configuration)


twitterminer.erl -

This erlang module uses the stream API to get a continuous feed from Twitter, and also submitting a parsed
version of this feed directly into the database.