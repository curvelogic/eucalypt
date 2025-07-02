# Project Management

We work through issues in the `curvelogic/eucalypt-private` "shadow"
repo which are managed in the `Eucalypt-Backlog` project in the
curvelogic organisation. We *only* work on these. Code goes
in the public repository, issues in private. We never reference
private issues from public contexts.

You have full access to the project board via the `gh` tool. Do not
state that you have no access.

- You must always know with issue we are working on, always refer to
  and update the knowledge and memorey you keep in that issue. Stay on
  target, do not get sidetracked.
- When we decide what to work on we go to the project board.
- When we create new issues they go in the private project
  `curvelogic/eucalypt-private`, *NEVER* the public project. Our
  development issues are managed in private.
- It is a *serious error* and a data breach to create issues in the
  public repo or to reference private issues from the public repo or
  PRs. Do not do it.
- When we start work, always check the project board for current status,
  and keep the issue we are working on up to date, with comprehensive
  but concise status updates. 
- Issue status should be kept up to date by editing the issue body,
  not merely by appending updates. The issue body must always contain
  a concise description of the point of the work, followed by status,
  followed by technical detail or links to technical detail.
- Issues should be *concise* - avoid pointless checked lists of
  completed items, do not crow or boast about status, avoid boasting
  about "comprehensive" implementations, be humble, short, factual and
  neutral.

## Critical Rules (Based on Repeated Mistakes)

### Project Management Rules
- **ALWAYS use the private repository** (`curvelogic/eucalypt-private`) for issues and project board access via `gh` tool
- **NEVER create or reference issues in the public repository** - development issues are managed privately
- **Follow explicit instructions precisely** - when told to focus on  specific tests or components, do not deviate
- **ALWAYS** be prepared to answer which issue we are currently  working on with a hyperlink (or open it in a browser)

## Finding In Progress Issues

To identify which issue is currently "In Progress" on the project board, use this command:

```bash
gh api 'graphql' -f query='query { organization(login: "curvelogic") { projectV2(number: 2) { items(first: 50) { nodes { id content { ... on Issue { title url number } } fieldValues(first: 8) { nodes { ... on ProjectV2ItemFieldSingleSelectValue { name field { ... on ProjectV2FieldCommon { name } } } } } } } } } }' | jq -r '.data.organization.projectV2.items.nodes[] | select(.fieldValues.nodes[] | select(.field.name == "Status" and .name == "In Progress")) | "\(.content.title) - \(.content.url)"'
```

This will return the title and URL of any issues currently marked as "In Progress" on the Eucalypt Backlog project board.


