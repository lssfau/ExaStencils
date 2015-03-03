import copy
import json
import os
import imp


def generate_configurations(configuration_class):
    ranged_parameter_configurations = [{"nameModifier": ""}]
    listed_parameter_configurations = [{"nameModifier": ""}]

    for param in sorted(configuration_class.rangedParameters):
        new_parameters = []
        it = configuration_class.rangedParameters[param][0]
        while it <= configuration_class.rangedParameters[param][1]:
            for config in ranged_parameter_configurations:
                new_config = copy.deepcopy(config)
                new_config[param] = it
                new_config["nameModifier"] += "_" + str(it).replace('"', '')
                new_parameters.append(new_config)
            it = configuration_class.rangedParameters[param][2](it)
        ranged_parameter_configurations = new_parameters

    for param in sorted(configuration_class.listedParameters):
        new_parameters = []
        for it in configuration_class.listedParameters[param]:
            for config in listed_parameter_configurations:
                new_config = copy.deepcopy(config)
                new_config[param] = it
                new_config["nameModifier"] += "_" + str(it).replace('"', '')
                new_parameters.append(new_config)
        listed_parameter_configurations = new_parameters

    print("Found %s configurations" % (len(ranged_parameter_configurations) + len(listed_parameter_configurations)))
    final_configs = []
    for ranged_config in ranged_parameter_configurations:
        for listed_config in listed_parameter_configurations:
            new_config = configuration_class()
            new_config.baseName = configuration_class.baseName + ranged_config["nameModifier"] + listed_config[
                "nameModifier"]
            new_config.constParameters = copy.deepcopy(configuration_class.constParameters)
            new_config.chosenRangedParameters = copy.deepcopy(ranged_config)
            del new_config.chosenRangedParameters["nameModifier"]
            new_config.chosenListedParameters = copy.deepcopy(listed_config)
            del new_config.chosenListedParameters["nameModifier"]
            new_config.update()
            if new_config.is_valid():
                final_configs.append(new_config)
    print("After filtering, %s valid configurations remain" % len(final_configs))
    return final_configs


def init_configurations(config_list_filename, configuration_class):
    configs = []

    if not '' == config_list_filename and os.path.isfile(config_list_filename):
        print("Reading configurations from list")
        with open(config_list_filename, 'r') as input_file:
            raw_data = json.load(input_file)

            for config in raw_data:
                new_config = configuration_class()
                new_config.baseName = config[0]
                new_config.constParameters = copy.deepcopy(configuration_class.constParameters)
                new_config.chosenRangedParameters = dict(config[1])
                new_config.chosenListedParameters = dict(config[2])
                new_config.update()
                configs.append(new_config)

            input_file.close()
    else:
        print("Generating configurations")
        configs = generate_configurations(configuration_class)

        if not '' == config_list_filename:
            print("Printing generated configs to list file")
            with open(config_list_filename, 'w+') as output_file:
                data = map(lambda c: [c.baseName, c.chosenRangedParameters, c.chosenListedParameters], configs)
                json.dump(data, output_file, indent=2)
                output_file.close()

    return configs


def load_config_class(config_file):
    if not os.path.isfile(config_file):
        print("Unable to load file:" + config_file)
        return

    print("Using project configuration " + config_file)

    # var_module = os.path.abspath(config_file)
    config = imp.load_source('vars', config_file)
    print("Successfully imported " + config_file)
    return config
