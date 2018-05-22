#!/usr/bin/env python3

import sys
from pathlib import Path
import re
import os
import subprocess
import tarfile
from github3 import login

def fetch(bindir):

    """ Download the latest binary release for this platform to `bindir`. """

    token = os.environ['GITHUB_API_TOKEN']
    if not token:
        raise EnvironmentError("No Github API Token available")

    gh = login(token=token)
    r = gh.repository("curvelogic", "eucalypt-hs")

    release = r.latest_release()
    asset = [a for a in release.assets() if "linux" in a.name][0]

    print("Downloading {}".format(asset.name))
    asset.download()

    print("Extracting {}".format(asset.name))
    with tarfile.open(asset.name, "r:gz") as tar:
        files = [f for f in tar if f.isreg()]
        print(" - extracting {}".format(f.name for f in files))
        tar.extractall(members = files)
        print(" - moving to ~/local/bin")
        for f in files:
            os.rename(f.name,  bindir / Path(f.name).name)
    
def main(args):

    if len(args) > 1:
        bindir = Path(args[1])
    else:
        bindir = Path.home() / "bin"

    fetch(bindir)

if __name__ == '__main__':
    main(sys.argv)
