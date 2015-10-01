-define(SENTRY_VERSION, 7).
-define(ESENTRY_VERSION, 0).
-define(CLIENT_NAME, <<"esentry">>).
-define(ESENTRY_USER_AGENT, io_lib:format("esentry/~B", [?ESENTRY_VERSION])).

-define(LEVEL_FATAL, <<"fatal">>).
-define(LEVEL_ERROR, <<"error">>).
-define(LEVEL_WARNING, <<"warning">>).
-define(LEVEL_INFO, <<"info">>).
-define(LEVEL_DEBUG, <<"debug">>).

-define(LOGER_NAME, <<"esentry">>).