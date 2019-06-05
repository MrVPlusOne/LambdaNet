from filters import *
import os
import subprocess as sp
import argparse
import json
import requests
import tarfile

filter_list = [filter_num_lines, filter_dep_avail]

parser = argparse.ArgumentParser(description="Finds repositories of the appropriate size, and retrieves .d.ts files")

parser.add_argument('--rootdir', dest="rootdir", help="Directory containig all the repositories in the format rootdir/username/repos")
parser.add_argument("--libdir", dest="libdir", help="Directory where all the .d.ts files should go")
parser.add_argument("--tmpdir", dest="tmpdir", help="Directory to clone dependencies which lack .d.ts files")

args = parser.parse_args()

freq = {}

class cd:
    """Context manager for changing the current working directory"""
    def __init__(self, newPath):
        self.newPath = os.path.expanduser(newPath)

    def __enter__(self):
        self.savedPath = os.getcwd()
        os.chdir(self.newPath)

    def __exit__(self, etype, value, traceback):
        os.chdir(self.savedPath)

def get_repos():
    users = os.listdir(args.rootdir)
    valid_repos = []
    for user in users:
        user_path = args.rootdir + "/" + user
        new_repos = [(user_path + "/" + r) for r in os.listdir(user_path)]
        for fil in filter_list:
            new_repos = filter(fil, new_repos)

        valid_repos.extend(new_repos)
        if len(valid_repos) == 100:
            print(valid_repos)
    return valid_repos
#
# tsc --declaration --outDir . --emitDeclarationOnly

def get_d_ts_files(valid_repos):
    for repo in valid_repos:
        deps = set()
        for (dirname, _, files) in os.walk(repo):
            for f in files:
                if f != "package.json":
                    continue
                try:
                    packages = json.load(open(os.path.join(dirname, f)))
                except:
                    continue

                if "dependencies" in packages:
                    deps = deps.union(set(packages["dependencies"].keys()))

#         if "devDependencies" in packages:
#             deps = deps.union(set(packages["devDependencies"].keys()))

        deps_ = set()

        for dep in deps:

            if dep not in freq:
                freq[dep] = 0

            freq[dep] += 1

            if dep[:6] != "@types":
                deps_.add("@types/" + dep)
            else:
                deps_.add(dep)

        for dep in deps_:
            dep_name = dep[len("@types/"):]
            github_url = "https://raw.githubusercontent.com/DefinitelyTyped/DefinitelyTyped/master/types/%s/index.d.ts" % dep_name

            d_ts = requests.get(github_url)
            if d_ts.status_code == 200:
                d_ts_txt = d_ts.text
                with open(args.libdir + "/" + dep_name + ".d.ts", "w+") as f:
                    f.write(d_ts_txt)

            else:
                npm_query = sp.run(["npm", "search", dep_name, "--json"], text=True, capture_output=True)
                if npm_query.returncode == 0:
                    npm_json = npm_query.stdout
                    search_results = json.loads(npm_json)

                    if len(search_results) == 0:
                        continue

                    result = search_results[0]
                    result_repo = result["links"]["repository"]
                    clone = sp.run(["git", "clone", result_repo, args.tmpdir + "/" + dep_name])
                    with cd(args.tmpdir + "/" + dep_name):
                        npm_install = sp.run(["npm", "install"])
                        gen_d_ts = sp.run(["tsc", "--declaration", "--outDir", ".", "--emitDeclarationOnly"], shell=True, executable="/bin/zsh")
                        print(gen_d_ts.args)

                    mv = sp.run(["mv", args.tmpdir + "/" + dep_name + "/**/*.d.ts", args.libdir], shell=True, executable="/bin/zsh")
                    print(mv.args)


def save_repos(repos):
    with tarfile.open("final.tgz", "w:gz") as f:
        for repo in repos:
            f.add(repo)

def main():
    repos = get_repos()
    print(repos)
    get_d_ts_files(repos)
    freq_ = sorted(freq.items(), key = lambda x: x[1])
    print(freq_)
    save_repos(repos)

if __name__ == "__main__":
    main()
