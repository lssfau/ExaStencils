from git import Repo
import json


def decorate_json(json_body: dict, problem_name: str, np: int, hostname: str):

    # git annotations
    repo = Repo(search_parent_directories=True)
    commit = repo.head.commit

    new_body = {'measurement': problem_name}
    new_body['tags'] = {}
    new_body['tags']['host'] = hostname
    new_body['fields'] = json_body
    new_body['fields']['commit'] = commit.hexsha
    new_body['fields']['np'] = np

    # must be list of dicts
    data = [new_body]

    return data


