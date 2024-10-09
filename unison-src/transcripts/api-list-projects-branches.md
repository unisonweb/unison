# List Projects And Branches Test

``` ucm :hide
scratch/main> project.create-empty project-one
scratch/main> project.create-empty project-two
scratch/main> project.create-empty project-three
project-one/main> branch branch-one
project-one/main> branch branch-two
project-one/main> branch branch-three
```

``` api
-- Should list all projects
GET /api/projects

-- Should list projects starting with project-t
GET /api/projects?prefix=project-t

-- Should list all branches
GET /api/projects/project-one/branches

-- Should list all branches beginning with branch-t
GET /api/projects/project-one/branches?prefix=branch-t
```
