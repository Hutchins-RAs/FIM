# Work Laptop Codex Handoff: FIM Social Benefits Haver Update

Date prepared: 2026-06-11

Audience: Codex running on Zixun's work laptop, using the work GPT account.

Purpose: explain what was developed on the personal laptop, how to get the same branch onto the work laptop, how to debug common transfer/setup issues, and how to test the Social Benefits Haver update in the work environment where the Brookings/Haver network path works.

## 1. Executive Summary

Zixun is testing a safer two-laptop workflow for the Hutchins Fiscal Impact Measure (FIM) project.

The personal laptop is being used for code development and Git organization. The work laptop is needed for the real Haver run because it has the working Brookings network/Haver access.

The current task branch is:

```text
codex/social-benefits-haver-bridge
```

The branch exists on Zixun's GitHub fork:

```text
https://github.com/zixuntonytan-new/FIM.git
```

The personal laptop branch is currently clean and pushed to `origin` at:

```text
282f6873b0a471241cca3aa89b1004f70aeb5899
282f687 Add Social Benefits Haver pull codes
```

Important: if the work laptop cannot see this branch or does not have the same files, first suspect a Git/folder/branch transfer problem. Do not assume the code itself is broken until the work laptop has been verified to be on the right repo, right remote, right branch, and right commit.

## 2. Repositories, Remotes, Branches, and Worktrees

Zixun's intended workflow has three separate roles:

```text
Personal laptop Codex branch:
  worktrees/codex-workbench
  branch: codex/social-benefits-haver-bridge
  purpose: Codex development branch for this Social Benefits Haver task

Personal laptop Claude branch:
  worktrees/claude-workbench
  purpose: Claude development/review work

Integration branch:
  worktrees/zixun_update_FIM
  branch: zixun_update_FIM
  purpose: final assembly/testing branch after reviewed work is approved

Clean baseline:
  Sarah_Chase_FIM
  branch: refactor/clean-data-pipeline
  purpose: clean official RA/Hutchins baseline checkout
```

Remote meanings:

```text
origin   = Zixun's fork, https://github.com/zixuntonytan-new/FIM.git
upstream = official Hutchins repo, https://github.com/Hutchins-RAs/FIM.git
```

Default rule:

```text
Push to origin only.
Do not push to upstream.
Do not merge into zixun_update_FIM until the task branch has run successfully on the work laptop.
```

## 3. What Changed on the Personal Laptop

The branch `codex/social-benefits-haver-bridge` changes the Haver pull workflow so Social Benefits Haver codes can be pulled through the same existing Haver pipeline.

Files changed in the current commit:

```text
data-raw/haver-pull.R
data/haver_names.xlsx
data/forecast_before_sb_haver_test.xlsx
```

Core intended code/data changes:

```text
data-raw/haver-pull.R
data/haver_names.xlsx
```

Important note:

```text
data/forecast_before_sb_haver_test.xlsx
```

is a safety backup workbook that ended up in the current branch. It is useful for local testing, but it is probably not desirable in the final PR. Before merging into the final contribution branch or opening a PR, consider removing this backup file from the branch.

### 3.1 `data/haver_names.xlsx`

The workbook now has:

```text
Sheet1
haver_names_SB
```

`Sheet1` remains the main old Haver name list.

`haver_names_SB` is the new Social Benefits add-on list. It currently contains:

```text
code     reference
yptss    social_security_nipa
yptv     veterans_benefits
gftfsx   supplemental_security_income
gftfrtx  refundable_tax_credits_from_social_benefits_new_c17
```

`gftfrtx` came from the existing forecast workbook, sheet `Social Benefits - NEW`, cell `C17`, where the row label is refundable tax credits. It has not been independently confirmed in live Haver yet. If the Haver run fails, this is one of the first names to inspect.

### 3.2 `data-raw/haver-pull.R`

The old script read only the default Haver names sheet:

```r
names_usna <- read_excel("data/haver_names.xlsx")
```

The updated script explicitly reads the old main sheet:

```r
names_usna <- read_excel("data/haver_names.xlsx", sheet = "Sheet1")
```

Then it reads the new Social Benefits sheet:

```r
names_usna_sb <- read_excel("data/haver_names.xlsx", sheet = "haver_names_SB")
social_benefits_haver_codes <- names_usna_sb$code
```

Then it combines the old and new code lists:

```r
usna_haver_codes <- unique(c(names_usna$code, names_usna_sb$code))
```

Then the Haver pull uses the combined list:

```r
pull_data(usna_haver_codes, "usna", start.date = START)
```

The script also relocates the Social Benefits add-on columns to the end of `national_accounts` before creating `Haver Pivoted`:

```r
relocate(any_of(social_benefits_haver_codes), .after = everything())
```

Reason: those rows should appear at the bottom of `Haver Pivoted`, making them easy to inspect and reducing the risk of shifting existing workbook formulas.

Finally, the script prints what it found in `Haver Pivoted` and warns if any Social Benefits code is missing.

Expected console message:

```text
Social Benefits add-on Haver codes: yptss, yptv, gftfsx, gftfrtx
Social Benefits add-on rows found in Haver Pivoted: yptss, yptv, gftfsx, gftfrtx
```

If one is missing, stop and debug before trusting the workbook.

## 4. The Meaning of "fatal: not a git repo"

If Zixun sees:

```text
fatal: not a git repository
```

it usually means the terminal is standing in the wrong folder.

Git commands must be run inside a cloned Git repository or inside a Git worktree. A normal folder is not enough.

Do not fix this by running:

```bash
git init
```

That would create a new empty repo and likely make the confusion worse.

Instead, first check where the terminal is:

```bash
pwd
ls -la
git rev-parse --show-toplevel
```

If `git rev-parse --show-toplevel` fails, the current folder is not inside the repo.

A valid repo/worktree folder usually contains either:

```text
.git/
```

or a `.git` file that points back to the shared worktree metadata.

On the personal laptop, the working repo folder is:

```text
C:\Users\zixun\OneDrive\Documents\Hutchins_code\FIM\worktrees\codex-workbench
```

But the work laptop will almost certainly have a different path. Do not blindly paste the personal laptop path on the work laptop.

In Git Bash, Windows paths look like this:

```bash
cd /c/Users/<WORK_USERNAME>/Documents/<some-folder>/<repo-folder>
```

For example:

```bash
cd /c/Users/zixun/Documents/Hutchins_code/FIM_work_laptop
```

Then run:

```bash
git rev-parse --show-toplevel
git status --short --branch
```

## 5. If the Work Laptop Does Not Have the Repo Yet

If there is no valid repo on the work laptop, clone Zixun's fork.

In Git Bash:

```bash
mkdir -p ~/Documents/Hutchins_code
cd ~/Documents/Hutchins_code
git clone https://github.com/zixuntonytan-new/FIM.git FIM_work_laptop
cd FIM_work_laptop
```

Then verify:

```bash
git remote -v
git status --short --branch
```

Expected `origin`:

```text
origin  https://github.com/zixuntonytan-new/FIM.git (fetch)
origin  https://github.com/zixuntonytan-new/FIM.git (push)
```

If `upstream` is missing and you want it for comparison later:

```bash
git remote add upstream https://github.com/Hutchins-RAs/FIM.git
git remote set-url --push upstream DISABLED
```

Do not push to upstream.

## 6. If the Repo Exists But the Branch Is Missing

From inside the correct repo folder:

```bash
git remote -v
git fetch origin --prune
git branch -a
```

Look for:

```text
remotes/origin/codex/social-benefits-haver-bridge
```

If the remote branch exists but no local branch exists yet:

```bash
git switch -c codex/social-benefits-haver-bridge --track origin/codex/social-benefits-haver-bridge
```

If the local branch already exists:

```bash
git switch codex/social-benefits-haver-bridge
git pull --ff-only origin codex/social-benefits-haver-bridge
```

Verify the branch and commit:

```bash
git status --short --branch
git rev-parse HEAD
git log --oneline --decorate -5
git show --name-only --oneline HEAD
```

Expected commit:

```text
282f6873b0a471241cca3aa89b1004f70aeb5899
282f687 Add Social Benefits Haver pull codes
```

Expected files in the commit:

```text
data-raw/haver-pull.R
data/haver_names.xlsx
data/forecast_before_sb_haver_test.xlsx
```

If the work laptop is not at this commit, something did not transfer yet. Check:

```bash
git fetch origin --prune
git remote -v
git branch -a
git status --short --branch
```

Also ask whether there were uncommitted changes left on the personal laptop. Git only transfers committed and pushed changes.

## 7. GitHub Desktop Notes for Work Laptop

GitHub Desktop should open a real repo folder, not the outer coordination folder.

Good choices:

```text
The work laptop clone folder, for example:
C:\Users\<WORK_USERNAME>\Documents\Hutchins_code\FIM_work_laptop
```

Do not open:

```text
An outer non-repo folder
A random old clone unless it has been checked
The personal laptop OneDrive path unless it truly exists on the work laptop
```

GitHub Desktop's "Open in VS Code" only opens the selected local folder. It does not commit, push, pull, fetch, or switch branches by itself.

If GitHub Desktop seems confused:

1. Check the repository path shown in GitHub Desktop.
2. Click "Fetch origin".
3. Check the current branch dropdown.
4. Switch to `codex/social-benefits-haver-bridge`.
5. Open the terminal from that exact folder and run:

```bash
git status --short --branch
git rev-parse HEAD
```

## 8. Before Running Haver on the Work Laptop

Close `data/forecast.xlsx` in Excel before running the R script.

Make a local backup of the workbook before running the Haver pull.

In Git Bash:

```bash
cp data/forecast.xlsx data/forecast_before_work_laptop_haver_run.xlsx
```

In PowerShell:

```powershell
Copy-Item "data\forecast.xlsx" "data\forecast_before_work_laptop_haver_run.xlsx"
```

Then verify the backup exists.

In Git Bash:

```bash
ls -lh data/forecast_before_work_laptop_haver_run.xlsx
```

In PowerShell:

```powershell
Get-Item "data\forecast_before_work_laptop_haver_run.xlsx"
```

## 9. Running the Haver Pull

Use RStudio or R from the repo root.

In RStudio:

```r
setwd("C:/path/to/the/work/laptop/FIM/repo")
getwd()
source("data-raw/haver-pull.R")
```

Replace the path with the actual work laptop repo path.

If using Git Bash or terminal and Rscript is available:

```bash
Rscript data-raw/haver-pull.R
```

But RStudio may be safer if that is how Haver access normally works on the work laptop.

Expected console messages include:

```text
Social Benefits add-on Haver codes: yptss, yptv, gftfsx, gftfrtx
Social Benefits add-on rows found in Haver Pivoted: yptss, yptv, gftfsx, gftfrtx
```

If the script errors before reaching these messages, debug the R/Haver environment first.

If it warns that a Social Benefits code is missing, debug the specific Haver code before trusting the workbook.

## 10. Workbook Checks After the Haver Pull

Open:

```text
data/forecast.xlsx
```

Check sheet:

```text
Haver Pivoted
```

At the bottom of the sheet, confirm the new rows exist:

```text
yptss
yptv
gftfsx
gftfrtx
```

These rows should appear at the bottom because the updated `haver-pull.R` relocates them there for auditability.

Then check:

```text
Social Benefits - NEW
```

Important rows:

```text
13  Social security                         yptss@usna
14  Veteran's benefits                      yptv@usna
16  SSI                                     GFTFSX@USNA
17  Refundable tax credits                  gftfrtx@usna
29  Social Security (NIPA Definition)       yptss@usna
36  Veteran's Benefits (with CBO rates)     yptv@usna
44  Supplemental Security Income            GFTFSX@USNA
```

The main `forecast` sheet does not read directly from `Haver Pivoted` for these lines. It reads:

```text
forecast row 17 -> Social Benefits - NEW row 8
forecast row 18 -> Social Benefits - NEW row 22
```

So the chain is:

```text
Haver pull
  -> Haver Pivoted
  -> Social Benefits - NEW calculations
  -> forecast rows 17 and 18
  -> final FIM code
```

## 11. Manual Excel Formula Bridge

The current R update makes the new Haver rows available in `Haver Pivoted`. It does not automatically rewrite all formulas in `Social Benefits - NEW`.

Conservative manual formula for a Social Benefits row, starting in `D13`:

```excel
=INDEX('Haver Pivoted'!$A:$ZZ,MATCH(LOWER(LEFT($C13,FIND("@",$C13)-1)),'Haver Pivoted'!$A:$A,0),MATCH(LOOKUP(2,1/($D$5:D$5<>""),$D$5:D$5)&" "&D$6,'Haver Pivoted'!$1:$1,0))
```

What it does:

```text
1. Reads the Haver code from column C, for example yptss@usna.
2. Strips off @usna and lowercases it.
3. Finds that code in Haver Pivoted column A.
4. Builds the quarter label from Social Benefits - NEW rows 5 and 6, for example 2025 Q4.
5. Finds the matching quarter column in Haver Pivoted row 1.
6. Returns the matching Haver Pivoted value.
```

Use this only for cells that should be actual Haver history. Do not overwrite projection cells unless Zixun explicitly decides that quarter should now be actual data.

For the current workbook, likely actual/history cells are around:

```text
D:P on Social Benefits - NEW
```

where:

```text
D = 2023 Q1
P = 2026 Q1
```

Projection cells after that may use CBO/manual growth-rate logic and should not be overwritten casually.

Rows most relevant to the new codes:

```text
13
14
16
17
29
36
44
```

After filling formulas, let Excel recalculate.

Then check:

```text
Social Benefits - NEW!O8:X8
Social Benefits - NEW!O22:X22
forecast!C17:L17
forecast!C18:L18
```

Those are the Social Benefits values the main forecast sheet feeds into the FIM pipeline.

## 12. Common Failure Modes and How to Think About Them

### 12.1 `fatal: not a git repository`

Likely cause:

```text
Wrong folder.
```

Fix:

```bash
pwd
ls -la
git rev-parse --show-toplevel
cd /path/to/actual/repo
```

Do not run `git init`.

### 12.2 Branch not found

Likely causes:

```text
The laptop has not fetched origin.
The repo's origin is not Zixun's fork.
The personal laptop branch was not pushed.
The terminal is in an old clone.
```

Debug:

```bash
git remote -v
git fetch origin --prune
git branch -a
git ls-remote --heads origin codex/social-benefits-haver-bridge
```

Expected origin:

```text
https://github.com/zixuntonytan-new/FIM.git
```

### 12.3 The code looks old or `haver_names_SB` is missing

Likely causes:

```text
Wrong branch.
Wrong commit.
Pull did not happen.
Personal laptop change was not committed/pushed.
```

Debug:

```bash
git status --short --branch
git rev-parse HEAD
git log --oneline --decorate -5
git show --name-only --oneline HEAD
```

Expected commit:

```text
282f687 Add Social Benefits Haver pull codes
```

### 12.4 Haver pull fails on one code

Likely causes:

```text
Code does not exist in USNA.
Code spelling/case is wrong.
Haver access is not available.
Brookings network path is unavailable.
```

First suspect:

```text
gftfrtx
```

because it came from `Social Benefits - NEW!C17` and needs live Haver validation.

Also test:

```text
yptss
yptv
gftfsx
```

### 12.5 R package or path errors

Likely causes:

```text
R working directory is wrong.
R packages are not installed.
The work laptop R environment differs from the personal laptop.
Haver network path only works in one R/session setup.
```

Debug:

```r
getwd()
list.files()
file.exists("data-raw/haver-pull.R")
file.exists("data/haver_names.xlsx")
```

Then source again:

```r
source("data-raw/haver-pull.R")
```

### 12.6 Excel opens with repair warnings

If Excel says it repaired external formula references or cached external-link records, do not panic immediately.

The workbook already contains external links such as:

```text
[3]Table - bill
```

The current simplified design does not intentionally add a new helper workbook layer. The key checks are:

```text
Does Haver Pivoted contain the new rows?
Do Social Benefits - NEW formulas/values look correct?
Do forecast rows 17 and 18 update as expected?
Did Excel remove formulas or only cached external-link metadata?
```

If Excel repair changes formulas in important sheets, stop and compare against the backup workbook.

## 13. Always Consider "Something Did Not Transfer"

When debugging on the work laptop, always consider this possibility:

```text
Something exists on the personal laptop but did not transfer to the work laptop.
```

Reasons this happens:

```text
The change was not committed.
The commit was not pushed.
The work laptop did not fetch.
The work laptop is on the wrong branch.
GitHub Desktop opened the wrong local folder.
The file is ignored by Git.
The file is binary Excel data and was edited locally but not committed.
The work laptop clone points to upstream instead of Zixun's fork.
```

Minimum transfer-debug checklist:

```bash
git remote -v
git status --short --branch
git rev-parse HEAD
git log --oneline --decorate -5
git show --name-only --oneline HEAD
git branch -a
```

Compare against personal laptop expected state:

```text
branch: codex/social-benefits-haver-bridge
HEAD:   282f6873b0a471241cca3aa89b1004f70aeb5899
remote: origin = https://github.com/zixuntonytan-new/FIM.git
```

If any of those differ, fix Git transfer/setup before debugging code.

## 14. What to Send Back to Personal-Laptop Codex

After testing on the work laptop, send back:

```text
1. The exact repo folder path on the work laptop.
2. Output of: git status --short --branch
3. Output of: git rev-parse HEAD
4. Output of: git remote -v
5. Full R console error or success messages from source("data-raw/haver-pull.R")
6. Whether Haver Pivoted contains yptss, yptv, gftfsx, gftfrtx.
7. Whether Social Benefits - NEW and forecast rows 17/18 look updated.
8. Whether data/forecast.xlsx changed.
9. Whether any manual Excel formulas were filled in, and which rows/columns.
```

If the Haver run succeeds and `data/forecast.xlsx` is changed, do not automatically merge to `zixun_update_FIM` yet. First decide whether to commit the workbook result on the task branch, whether to remove the backup workbook file, and whether the manual Excel formula bridge should become code or stay manual.

## 15. Later Integration Plan

Only after the task branch is tested on the work laptop:

```text
1. Review the resulting diff.
2. Remove test-only files if needed, especially forecast_before_* backups.
3. Decide whether updated forecast.xlsx should be committed.
4. Merge the tested task branch into zixun_update_FIM.
5. Push zixun_update_FIM to origin.
6. Run final checks from zixun_update_FIM.
7. Only then consider a PR from Zixun's fork to Hutchins-RAs/FIM.
```

Do not use `Sarah_Chase_FIM` as the integration branch. Keep it as the clean RA/Hutchins baseline.

