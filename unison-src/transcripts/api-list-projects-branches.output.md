# List Projects And Branches Test

``` api
--  Should list all projects
GET /api/projects
[
    {
        "projectName": "project-one"
    },
    {
        "projectName": "project-three"
    },
    {
        "projectName": "project-two"
    },
    {
        "projectName": "scratch"
    }
]
--  Should list projects starting with project-t
GET /api/projects?prefix=project-t
[
    {
        "projectName": "project-three"
    },
    {
        "projectName": "project-two"
    }
]
--  Should list all branches
GET /api/projects/project-one/branches
[
    {
        "branchName": "branch-one"
    },
    {
        "branchName": "branch-three"
    },
    {
        "branchName": "branch-two"
    },
    {
        "branchName": "main"
    }
]
--  Should list all branches beginning with branch-t
GET /api/projects/project-one/branches?prefix=branch-t
[
    {
        "branchName": "branch-three"
    },
    {
        "branchName": "branch-two"
    }
]
```

