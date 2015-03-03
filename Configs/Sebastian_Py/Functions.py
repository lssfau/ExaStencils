import copy
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
                new_config["nameModifier"] += "_" + str(it)
                new_parameters.append(new_config)
            it = configuration_class.rangedParameters[param][2](it)
        ranged_parameter_configurations = new_parameters

    for param in sorted(configuration_class.listedParameters):
        new_parameters = []
        for it in configuration_class.listedParameters[param]:
            for config in listed_parameter_configurations:
                new_config = copy.deepcopy(config)
                new_config[param] = it
                new_config["nameModifier"] += "_" + str(it)
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


def load_config(config_file):
    if not os.path.isfile(config_file):
        print("Unable to load file:" + config_file)
        return

    print("Using project configuration " + config_file)

    # var_module = os.path.abspath(config_file)
    config = imp.load_source('vars', config_file)
    print("Successfully imported " + config_file)
    return config
