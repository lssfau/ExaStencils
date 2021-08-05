# Git Guidelines

## Permissions

* Expiration dates for students:
  * Bachelor: 8 months as Developer
  * Master: 12 months as Developer

## Conventions for Branches

### Naming Conventions

* Student theses or projects: `/student/xyz`
* Development branches, e.g. features or HiWi projects: `/devel/xyz`
* Archive branches: `archive/xyz`
* Release branches: `X-stable`

### General Conventions

* Branches should only contain new features that belong to their respective topic to avoid tedious cherry-picking of certain commits, e.g. a /devel/timing branch should have commits containing fixes for visualization statements
* Add entries to the `.gitignore` file to circumvent bloating the repository with binaries, logs and other unwanted files

## Releases

This procedure should be followed whenever a new release is published:

1. Push `Bump to version X.Y` commit on master (e.g. `Bump to version 5.1`)
2. Create new branch with name `X-stable` (e.g. `5-stable`)
3. Create new release with tag `X.Y` and title `vX.Y` (e.g. tag `5.1` and title `v5.1`)
4. Push `Bump to version X+1.0` commit on master (e.g. `Bump to version 6.0`).
5. Create new tag from this commit with name `vX+1.0dev` (e.g. `v6.0dev`).
6. Update link to release on ExaStencils website: https://www.exastencils.fau.de/
