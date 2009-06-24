#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    code:add_pathz("src/couchdb"),
    etap:plan(12),
    
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    
    ok.

check_status(Pid,ListPropLists) ->
    From = list_to_binary(pid_to_list(Pid)),
    Element = lists:foldl(fun(PropList,Acc) ->
				     case proplists:get_value(pid,PropList) of
					 From -> [PropList | Acc];
					 _ -> []
				     end
			     end,[],ListPropLists),
    proplists:get_value(status,hd(Element)).

loop() ->
    receive
	{add, From} ->
	    couch_task_status:add_task("type","task","init"),
	    From ! {ok, self()},
	    loop();
	{update, Status, From} ->
	    couch_task_status:update(Status),
	    From ! {ok, self()},
	    loop();
	{update_frequency, Msecs, From} ->
	    couch_task_status:set_update_frequency(Msecs),
	    From ! {ok, self()},
	    loop();
	{done, From} ->
	    From ! {ok, self()}
    end.

call(Pid, Command) ->
    Pid ! {Command, self()},
    wait().

call(Pid, Command, Arg) ->
    Pid ! {Command, Arg, self()},
    wait().

wait() ->
    receive
        _ -> ok
    after 1000 ->
        etap:is(true, false, "Timed out waiting for pid to respond.")
    end.    

test() ->   
    {ok, TaskStatusPid} = couch_task_status:start_link(),
    
    

    TaskUpDater = fun() -> loop() end,
    % create three updaters
    Pid1 = spawn(TaskUpDater),
    Pid2 = spawn(TaskUpDater),
    Pid3 = spawn(TaskUpDater),
    % add the first
    call(Pid1,add),

    etap:is(length(couch_task_status:all()),1,"one task is running."),    
    etap:is(check_status(Pid1,couch_task_status:all()),<<"init">>,
		"status should be initialized."),

    call(Pid1,update,"running"),

    etap:is(check_status(Pid1,couch_task_status:all()),<<"running">>,
		"status should be running."),

    call(Pid2,add),
    etap:is(length(couch_task_status:all()),2,"two tasks are running."),
    etap:is(check_status(Pid2,couch_task_status:all()),<<"init">>,
		"status of second should be initialized."),
    call(Pid2,update,"running"),
    etap:is(check_status(Pid2,couch_task_status:all()),<<"running">>,
		"status should be running."),


    call(Pid3,add),
    etap:is(length(couch_task_status:all()),3,"two tasks are running."),
    etap:is(check_status(Pid3,couch_task_status:all()),<<"init">>,
		"status of second should be initialized."),
    call(Pid3,update,"running"),
    etap:is(check_status(Pid3,couch_task_status:all()),<<"running">>,
		"status should be running."),

    
    call(Pid3,update_frequency,500),
    call(Pid3,update,"still running"),
    etap:is(check_status(Pid3,couch_task_status:all()),<<"still running">>,
		"status should be running."),

    call(Pid3,update,"are we there yet daddy"),
    etap:is(check_status(Pid3,couch_task_status:all()),<<"still running">>,
		"no we're not there yet."),

    call(Pid3,update_frequency,0),
    call(Pid3,update,"are we there"),
    etap:is(check_status(Pid3,couch_task_status:all()),<<"are we there">>,
		"status should now be are we there."),    


    call(Pid1,done),
    etap:is(length(couch_task_status:all()),2,"two tasks are running."),
    
    call(Pid2,done),
    etap:is(length(couch_task_status:all()),1,"one tasks is running."),
    call(Pid3,done),
    etap:is(length(couch_task_status:all()),0,"no tasks are running."),

    % test gen_server stop
    erlang:monitor(process, TaskStatusPid),
    couch_task_status:stop(),
    receive
        {'DOWN', _, _, TaskStatusPid, _} ->
	    etap:diag("task_status stopped",[]),
	    ok;
        _Other -> etap:diag("OTHER: ~p~n", [_Other])
    after
        1000 -> throw({timeout_error, task_status_stop})
    end,    
	    
    ok.
