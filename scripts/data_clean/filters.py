import subprocess as sp
import json
import requests
import os

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

def check_pkg_json(packages, internals):
    deps = set()

    if "dependencies" in packages:
        deps = deps.union(set(packages["dependencies"].keys()))

#         if "devDependencies" in packages:
#             deps = deps.union(set(packages["devDependencies"].keys()))


    if "name" in packages:
        internals.add(packages["name"])

    deps_ = set()

    for dep in deps:

        if dep[:6] != "@types":
            deps_.add("@types/" + dep)
        else:
            deps_.add(dep)

    for dep in deps_:
        dep_name = dep[len("@types/"):]
        github_url = "https://raw.githubusercontent.com/DefinitelyTyped/DefinitelyTyped/master/types/%s/index.d.ts" % dep_name

        d_ts = requests.get(github_url)
        if d_ts.status_code != 200 and (dep_name not in internals):
            return False

    return True


def filter_dep_avail(repo):
    # try:
    #     packages = json.load(open(repo + "/" + "package.json"))
    # except:
    #     return True

    for (dirname, _, files) in os.walk(repo):
        internals = set()
        for filename in files:
            if filename == "package.json":
                try:
                    packages = json.load(open(os.path.join(dirname, filename)))
                    if not check_pkg_json(packages, internals):
                        return False
                except:
                    continue

    return True
