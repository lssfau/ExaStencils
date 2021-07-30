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

### General Conventions

* Branches should only contain new features that belong to their respective topic to avoid tedious cherry-picking of certain commits, e.g. a /devel/timing branch should have commits containing fixes for visualization statements
* Add entries to the `.gitignore` file to circumvent bloating the repository with binaries, logs and other unwanted files
