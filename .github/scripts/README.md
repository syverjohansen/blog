# Hosted automation helpers

## FantasyXC roster email

`fantasyxc_roster_digest.py` renders the guarded automatic roster into plain
text and HTML tables grouped by country and event. It also lists evidence that
was not safe to apply and therefore uses the historical/quota fallback.

The `Cross-country FantasyXC roster evidence` workflow sends a message only
when the SHA-256 digest of that roster contract changes. Official FIS
startlists remain authoritative downstream.

One repository secret is required under **Settings → Secrets and variables →
Actions**:

- `FANTASYXC_EMAIL_APP_PASSWORD`: a Gmail app password for the sender account.

The sender and recipient default to `syverjohansen@gmail.com`. To use another
address, set the repository variable `FANTASYXC_NOTIFICATION_EMAIL`. That
account must match the Gmail account that issued the app password.

Run the offline tests from the scripts directory:

```bash
cd .github/scripts
python3 -m unittest -v test_fantasyxc_roster_digest.py
```

## Daily results and athlete identity reviews

The result lifecycles for all five sports run `daily_results_digest.py` even
when processing or publication fails. They reuse the Gmail secret and address
above. Each message links the workflow, reports stage outcomes, lists every
unresolved identity and automatic newcomer, and links still-open reviews.
A missing identity report is shown as missing evidence, never as zero failures.

Review issues live in the blog repository with the `athlete-identity` label.
They are keyed by sport, sex, and stable FIS code or IBU ID. No-change runs keep
open reviews visible; closing an issue confirms a genuine debut and prevents
reopening it on every run. A reviewed alias observed by a later run closes the
matching issue automatically. Suggestions are evidence for human review and
never cause a fuzzy merge.

For a duplicate athlete, verify the official source ID and existing historical
ID, add the alias in ski, replay the affected races (all competitors' ratings
are affected), then regenerate the site. Cross-country has an isolated
`cross_country_identity_repair.py` command for repairing an open individual
season; it preserves prior-season CSV bytes and the initial pre-race ratings
of genuine newcomers. Inspect the candidate before replacing accepted history.

Daily emails and full identity evidence are retained as workflow artifacts.
Reporting failure fails the workflow; it cannot silently claim a clean run.
Biathlon now acquires finalized IBU results before publishing site data and
uses the same date cutoff for acquisition and publication.
