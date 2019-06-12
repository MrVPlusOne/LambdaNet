import subprocess as sp
import json
import requests
import os
from pathlib import Path

def filter_num_lines(repo):
    cloc = sp.run(["cloc", repo], text=True, capture_output=True)
    cloc_out = cloc.stdout
    for line in cloc_out.splitlines():
        l = [x.strip() for x in line.split()]
        if (len(l) == 0):
            continue
        if l[0] == "TypeScript":
            num_lines = int(l[4])
            if 500 <= num_lines <= 5000:
                return True

    return False

def check_pkg_json(packages, internals, def_typed_url):
    deps = set()

    if "dependencies" in packages:
        deps = deps.union(set(packages["dependencies"].keys()))

    if "peerDependencies" in packages:
        deps = deps.union(set(packages["peerDependencies"].keys()))

    deps_ = set()

    for dep in deps:

        if dep[:6] != "@types":
            deps_.add("@types/" + dep)
        else:
            deps_.add(dep)

    for dep in deps_:
        dep_name = dep[len("@types/"):]
        dep_dir_url = def_typed_url + "/types/" + dep_name
        if not (Path(dep_dir_url).is_dir()) and (dep_name not in internals):
            return False

    return True


def filter_dep_avail_(def_typed_dir, repo):
    internals = set()
    for (dirname, _, files) in os.walk(repo):
        for filename in files:
            if filename == "package.json":
                try:
                    packages = json.load(open(os.path.join(dirname, filename)))
                    if "name" in packages:
                        internals.add(packages["name"])
                except:
                    continue
    for (dirname, _, files) in os.walk(repo):

        for filename in files:
            if filename == "package.json":
                try:
                    packages = json.load(open(os.path.join(dirname, filename)))
                    if not check_pkg_json(packages, internals, def_typed_dir):
                        return False
                except:
                    continue

    return True


def filter_dep_avail(definitely_typed_dir):
    return lambda repo: filter_dep_avail_(definitely_typed_dir, repo)
    # try:
    #     packages = json.load(open(repo + "/" + "package.json"))
    # except:
    #     return True
