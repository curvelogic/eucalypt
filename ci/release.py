#!/usr/bin/env python3

import sys
import pathlib
import re
import os
import subprocess
import tarfile
from github3 import login

def release(package, commit, version):

    """ Create or update a github release for this version / commit, uploading package. """

    print("Releasing {} from commit {} as version {}".format(package, commit, version))

    token = os.environ['GITHUB_API_TOKEN']
    if not token:
        raise EnvironmentError("No Github API Token available")
    gh = login(token=token)
    r = gh.repository("curvelogic", "eucalypt")

    try:
        release = r.release_from_tag(version)
        print("Found existing release for {}".format(version))
    except:
        print("Creating release for {}".format(version))
        release = r.create_release(tag_name = version,
                                   name = version,
                                   body = "Prototype eu binary.",
                                   target_commitish = commit,
                                   draft = True,
                                   prerelease = True)

    if release:
        print("Uploading binary {}".format(package))
        release.upload_asset(content_type = "application/binary",
                             name = package,
                             asset = open(package, 'rb'))

def main(args):

    """ Determine version details to release and then create. """

    exe_path = pathlib.Path(args[1])

    # Determine architecture from path
    arch_tags = [item for item in exe_path.parts if re.match('.*64.*', item)]
    if arch_tags:
        arch = arch_tags[0]
    else:
        arch = "x86_64-osx"

    # Query eu itself for version details
    version = subprocess.check_output([exe_path, "-e", "eu.build.version"]).strip().decode('utf8').strip("'")
    # Until this whole circular thing is operational
    # commit = subprocess.check_output([exe_path, "-e", "eu.build.commit"]).strip().decode('utf8').strip("'")
    commit = os.environ.get("CIRCLE_SHA1")
    if not commit:
        commit = subprocess.check_output(["git", "rev-parse", "HEAD"]).strip().decode('utf8').strip("'")

    # TGZ the exe
    package = "eucalypt-hs-" + arch + ".tgz"
    with tarfile.open(package, "w:gz") as tar:
        tar.add(exe_path)

    release(package, commit, version)

if __name__ == '__main__':
    main(sys.argv)
