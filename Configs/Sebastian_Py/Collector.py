from collections import defaultdict
import sys
import getopt
import time
from time import gmtime, strftime

import Functions


basePath = "C:\\Users\\sisekuck\\Documents\\Visual Studio 2010\\Projects\\ScalaExaStencil\\Heap\\results"


def read_timings(configurations):
    timings_raw = dict()
    for config in configurations:
        try:
            timings_per_thread = []
            with open(basePath + "/" + config.baseName + "/timings.csv", "r") as timing_file:
                for line in timing_file:
                    line = line.strip()
                    words = line.split(";")
                    separate_timers = []
                    for i in range(0, len(words) - 1, 3):
                        separate_timers.append([words[i].strip(), float(words[i + 1]), float(words[i + 2])])
                    timings_per_thread.append(separate_timers)
                timing_file.close()
            timings_raw[config.baseName] = timings_per_thread
        except IOError:
            print("Invalid configuration: " + config.baseName)
    return timings_raw


def slash_timings(timings_raw, how):  # 'how' may be 'MIN', 'MAX' or 'MEAN'
    slashed_timings = dict()
    for timed_config in timings_raw:
        timings_per_thread = timings_raw[timed_config]
        if "MIN" == how:
            slashed = []
            for timer_idx in range(0, len(timings_per_thread[0]) - 1):
                slashed.append([timings_per_thread[0][timer_idx][0],
                                min(t[timer_idx][1] for t in timings_per_thread),
                                min(t[timer_idx][2] for t in timings_per_thread)])
            slashed_timings[timed_config] = slashed
        elif "MAX" == how:
            slashed = []
            for timer_idx in range(0, len(timings_per_thread[0]) - 1):
                slashed.append([timings_per_thread[0][timer_idx][0],
                                max(t[timer_idx][1] for t in timings_per_thread),
                                max(t[timer_idx][2] for t in timings_per_thread)])
            slashed_timings[timed_config] = slashed
        if "MEAN" == how:
            slashed = []
            for timer_idx in range(0, len(timings_per_thread[0]) - 1):
                slashed.append([timings_per_thread[0][timer_idx][0],
                                sum(t[timer_idx][1] for t in timings_per_thread) / len(timings_per_thread),
                                sum(t[timer_idx][2] for t in timings_per_thread) / len(timings_per_thread)])
            slashed_timings[timed_config] = slashed
        else:
            print("ERROR: Invalid choice for how: " + how)
    return slashed_timings


def create_dict(slashed_timings, how, what):  # 'how' may be 'MIN', 'MAX' or 'MEAN'; 'what' may be 'MEAN' or 'TOTAL'
    index = 0
    if "TOTAL" == what:
        index = 1
    elif "MEAN" == what:
        index = 2
    else:
        print("ERROR: Invalid choice for what: " + what)

    if "MIN" == how:
        reduced_timings = defaultdict(lambda: [sys.maxsize, ""])
        for timed_config in slashed_timings:
            for timing in slashed_timings[timed_config]:
                if reduced_timings[timing[0]][0] > timing[index]:
                    reduced_timings[timing[0]] = [timing[index], timed_config]
        return reduced_timings
    elif "MAX" == how:
        reduced_timings = defaultdict(lambda: [0, ""])
        for timed_config in slashed_timings:
            for timing in slashed_timings[timed_config]:
                if reduced_timings[timing[0]][0] < timing[index]:
                    reduced_timings[timing[0]] = [timing[index], timed_config]
        return reduced_timings
    elif "MEAN" == how:
        reduced_timings = defaultdict(lambda: [0, ""])
        for timed_config in slashed_timings:
            for timing in slashed_timings[timed_config]:
                reduced_timings[timing[0]][0] += timing[index]
        for red in reduced_timings:
            reduced_timings[red][0] /= len(slashed_timings)
        return reduced_timings
    else:
        print("ERROR: Invalid choice for how: " + how)


def main(argv):
    project_filename = ''
    config_list_filename = ''
    opts, args = getopt.getopt(argv, "p:c:")
    for opt, arg in opts:
        if opt == '-p':
            project_filename = arg
        elif opt == '-c':
            config_list_filename = arg
        else:
            print('Unknown option:' + opt + arg)
            sys.exit()

    if '' == project_filename:
        print('No project configuration provided.')
        return

    start_time = gmtime()
    print("Starting at: " + strftime("%Y-%m-%d %H:%M:%S", start_time))

    print("Loading project configuration")
    parameters = Functions.load_config_class(project_filename)

    # get configs
    configs = Functions.init_configurations(config_list_filename, parameters.Configuration)

    print("Reading timing data")
    timings_raw = read_timings(configs)
    print("%s of %s configs were valid" % (len(timings_raw), len(configs)))

    print("Slashing timing data")
    slashed_timings = slash_timings(timings_raw, "MEAN")

    print("Generating statistics")
    timings_min = create_dict(slashed_timings, "MIN", "TOTAL")
    timings_max = create_dict(slashed_timings, "MAX", "TOTAL")
    timings_mean = create_dict(slashed_timings, "MEAN", "TOTAL")

    print("Printing results")
    print("\n")
    for timing in sorted(timings_min):
        print("Min for timer %s: %s (config %s)" % (timing, timings_min[timing][0], timings_min[timing][1]))
    print("\n")
    for timing in sorted(timings_max):
        print("Max for timer %s: %s (config %s)" % (timing, timings_max[timing][0], timings_max[timing][1]))
    print("\n")
    for timing in sorted(timings_mean):
        print("Mean for timer %s: %s" % (timing, timings_mean[timing][0]))
    print("\n")

    print("\n\nFinished at: " + strftime("%Y-%m-%d %H:%M:%S", gmtime()))
    delta_time = time.mktime(gmtime()) - time.mktime(start_time)
    print("Total time required: " + str(delta_time) + " s.")


main(sys.argv[1:])