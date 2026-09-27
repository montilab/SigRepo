# Contributing

These rules apply to [SigRepo](https://github.com/montilab/SigRepo) and
[SigRepo_Server](https://github.com/montilab/SigRepo_Server). They are meant to be
followed rather than read, so they are short.

The point of all of them: someone outside the project should be able to open the
tracker and tell, without asking, what is being worked on, what is waiting, and
what has been decided.

## The rules

**Every change starts as an issue.** Create the branch from the issue, so the two
are linked before any code is written:

```sh
gh issue create --repo montilab/SigRepo --title "..." --body "..."
gh issue develop <number> --base dev --checkout
```

Branch names carry no type prefix — not `fix/`, not `feat/`, not `chore/`. The
default `<number>-<title-slug>` that `gh` produces is what we want.

**`dev` is what gets worked on; `master` is what is live.** Branches come off
`dev` and pull requests go into `dev`. `dev` is deployed to the staging instance
at montilab.bu.edu, where a change is exercised against a full replica of the
data before anyone outside sees it.

`master` is production: it is what sigrepo.org runs. It gains commits only
through a release pull request from `dev`, and direct pushes to it are blocked.
If something ever has to be hotfixed straight onto `master`, merge `master` back
into `dev` the same day. That one rule is what stops the two branches drifting
apart, which is how this arrangement usually goes wrong.

A change that spans both repositories is released from both or from neither.
Otherwise production runs a client and a server from different lines, and the
resulting bug report makes no sense to anyone.

**Every pull request closes its issue.** Put `Closes #NNN` in the description.
This is the rule the rest depends on: it is what lets the board move by itself and
what leaves the reason for a change next to the change.

Note when it fires. GitHub auto-closes an issue only when the pull request merges
into the repository's default branch, which is `master`. Pull requests target
`dev`, so `Closes #NNN` does nothing at the moment your pull request merges: the
issue closes later, when the release pull request carries `dev` into `master`.
Write it anyway, in every pull request. It means the board shows what is live
rather than what is merged, which is usually the question being asked of it.

**Ideas and work are different things.** An idea states a goal, is labelled `idea`,
and is never closed by a pull request. A task or bug is real work: it links to the
idea it serves, and it is closed by the pull request that resolves it. An idea is
closed only when it has been decided against, or when the work it called for is
done and its issues are closed.

**A bug report says what was run, what happened, and the error.**
[SigRepo#75](https://github.com/montilab/SigRepo/issues/75) is the standard to copy.
The bug form asks for exactly these three things.

**Every issue carries one type label and one status label.** If neither fits, the
issue is not ready — see the table below.

**Branches are short-lived and merged on GitHub.** Not locally, so the review and
the history stay in one place. Delete the branch when its pull request merges.

**Nothing is closed silently.** Say what happened and link the pull request or the
commit. A closed issue with no explanation is a question someone has to ask later.

**The board is a by-product, not a chore.** Status on
[the project board](https://github.com/orgs/montilab/projects/4) is set by
automation. If you find yourself updating it by hand, something upstream is wrong —
usually a missing `Closes #NNN` or an issue that was never added to the board.

**If you cannot state the next step, the issue is not ready.** Write the next step
in the issue, or label it `needs-decision` and say what the decision is.

**Stale means ninety days.** Untouched issues get asked about once, then closed if
nobody answers. Closing is not a judgement on the idea; reopen it when it matters
again.

## Reaching the staging instance

montilab.bu.edu keeps every port bound to `127.0.0.1`. It is a shared,
BU-managed host, the lab has already been warned once about an exposed port, and
the staging database holds real signatures behind accounts that can edit them.
So nothing there is published to the network, and reaching it means your own SSH
tunnel with your own BU account:

```sh
ssh -N -o ExitOnForwardFailure=yes \
  -L 9051:127.0.0.1:8051 -L 9050:127.0.0.1:8050 -L 9020:127.0.0.1:8020 montilab
```

with `Host montilab` proxying through `scc4.bu.edu` — not scc2, which has no
route to the BUMC network. Local ports are offset by 1000 so they do not collide
with a stack running on your own machine. If you already have a ControlMaster
session open, that command returns immediately and the master process holds the
forwards: the tunnel is live even though it looks like the command exited.

Then open <http://127.0.0.1:9051>.

This needs SCC access. If you do not have it, ask for a walkthrough rather than
a port.

## Labels

Exactly one type label, and exactly one status label.

| Type | Use for |
| --- | --- |
| `bug` | Something is broken. What was run, what happened, the error. |
| `enhancement` | New behaviour or a change to existing behaviour. |
| `documentation` | Vignettes, README, roxygen, this file. |
| `idea` | A goal, not a task. Never closed by a pull request. |

| Status | Meaning |
| --- | --- |
| `ready` | The next step is stated and anyone could pick it up. |
| `needs-decision` | Blocked on a choice a person has to make. Say which choice. |
| `blocked` | Blocked on something outside this issue. Link it. |

Two more labels exist for specific jobs:

- `no-changelog` — the change is not user-visible, so it needs no `NEWS.md` entry.
- `data-quality` — the code is fine; the data in the repository is wrong.

## Pull requests

- `Closes #NNN` in the description.
- Add a `NEWS.md` bullet under `# SigRepo (development version)` for anything a
  user would notice, or apply `no-changelog`.
- Merge on GitHub once review passes. Delete the branch.

## Where work happens

Local checkouts stay on `master`; branch work goes in a worktree per issue. The
`sr-wt new <repo> <issue>` helper creates one, and `sr-wt prune` removes it once
the pull request merges.
