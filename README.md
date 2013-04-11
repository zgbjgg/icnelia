==== icnelia ==== 
====

 ***icnelia***: is a word in nahuatl language that means "help"

 Helper for compile, clean and create run scripts for your erlang OTP application

### ===== how to start =====

 Clone icnelia repository from github:

	$ git clone https://github.com/jorgegarrido/icnelia.git

 Move into icnelia directory: ```$ cd icnelia``` and build the project and script: ```./buiicn```, now you can use 
 the icnelia script in your Erlang OTP Applications.

### ===== how to use =====

 Just place the icnelia script into your main directory of your application to compile use ```./icnelia compile``` and 
 compiles all the code in src directory, also set the compiled files (.beam) under ebin directory, to 
 clean use ```./icnelia clean``` and cleans all the compiled code in ebin directory.

 To create a run script use ```./icnelia runner``` and creates a run script to start your application 
 (only start and load an erlang shell).

 To create a run daemon script use ```./icnelia runner daemon``` and creates a run script to start your application 
 as daemon

### ===== icnelia.config =====
 
 ***app_deps***: here you can define the paths to ebin deps, for example:
		
	{app_deps, [{"MY_DEP", "my_dep/ebin"}]}.

 where "MY_DEP" is the name that the run script use to recongnize the path to your dep, and "my_dep/ebin" is the path 
 to ebin for the dependence

 ***pipes_dir***: here you can define the path to your pipes directory, it is used when you want to run your application as daemon, for example:

	    {pipes_dir, "/usr/local/My_Pipes/"}.	    
	    NOTE: YOU MUST CREATE THE PIPES DIRECTORY MANUALLY
 
 ***logs_dir***: here you can define the path to logs used by your application, when it is running as daemon, for example:

	   {logs_dir, "/usr/local/My_Logs/"}.
	   NOTE: YOU MUST CREATE THE LOGS DIRECTORY MANUALLY

 ***erl_opts***: here you can set the options to compile the source code, the options are all supported by erlang version running, for example:

	   {erl_opts, [debug_info]}.

### ===== LICENSE =====
 
 see LICENSE.txt for more info
