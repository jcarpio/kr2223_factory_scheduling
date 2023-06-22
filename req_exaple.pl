slots_per_week(120). 
slots_per_day(24).

tool_task_worker_times('milling machine', mill, alex, 24).
worker_freeday(alex, 1).
worker_day_availability(alex, [1,1,1,1,1,1,0]).

tool_task_worker_times('drilling machine', drill, sarah, 24).
worker_freeday(sarah, 1).
worker_day_availability(alex, [1,1,1,1,1,0,1]).

tool_task_worker_times('turning machine', turn, sele, 24).
worker_freeday(sele, 1).
worker_day_availability(sele, [1,1,1,1,0,1,1]).

tool_task_worker_times('assembling machine', assemble, aron, 24).
worker_freeday(aron, 1).
worker_day_availability(aron, [1,1,1,0,1,1,1]).

tool_task_worker_times('coating machine', varnish, noah, 24).
worker_freeday(noah, 1).
worker_day_availability(noah, [1,1,0,1,1,1,1]).

tool_task_worker_times('washing_machine', wash, emilie, 24).
worker_freeday(emilie, 1).
worker_day_availability(emilie, [1,0,1,1,1,1,1]).

tool_task_worker_times('drying_machine', dry, jed, 24).
worker_freeday(jed, 1).
worker_day_availability(jed, [0,1,1,1,1,1,1]).