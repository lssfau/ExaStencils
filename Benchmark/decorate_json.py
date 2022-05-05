from git import Repo
import json


def decorate_json(json_body: dict, np: int, hostname: str):

    # git annotations
    repo = Repo(search_parent_directories=True)
    commit = repo.head.commit

    json_body['fields'] = {}
    json_body['fields']['commit'] = commit.hexsha
    json_body['fields']['np'] = np
    json_body['tags'] = {}
    json_body['tags']['host'] = hostname

    print(json_body)

    return json_body


