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
