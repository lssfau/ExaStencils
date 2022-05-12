from git import Repo
import json
from config_from_knowledge import *

######################
# - json functions - #
######################


def decorate_json(json_body: dict, config: ConfigFromKnowledge):

    # git annotations
    repo = Repo(search_parent_directories=True)
    commit = repo.head.commit

    new_body = {'measurement': config.problem_name, 'tags': {}}
    new_body['tags']['host'] = config.host_name
    new_body['tags']['gpu'] = config.gpu_name
    new_body['tags']['cpu'] = config.cpu_name
    new_body['tags']['cuda_enabled'] = config.use_cuda
    new_body['tags']['mpi_numThreads'] = config.mpi_num_processes
    new_body['tags']['omp_numThreads'] = config.omp_num_threads
    new_body['tags']['numNodes'] = config.n_nodes
    new_body['tags']['numBlocks'] = config.n_blocks
    new_body['tags']['simd'] = config.simd_instructionSet
    new_body['tags']['commit'] = commit.hexsha
    new_body['fields'] = json_body

    # must be list of dicts
    data = [new_body]

    return data

