# GitHub Actions + SSH Setup Guide

This guide will set up free automation that runs even when your laptop is closed, using GitHub Actions to connect to your Mac via SSH.

## Step 1: Enable SSH on Your Mac

1. Open **System Preferences** → **Sharing**
2. Check the box next to **Remote Login** to enable SSH
3. Note your username (shown in the Remote Login section)

## Step 2: Generate SSH Key for GitHub Actions

Open Terminal and run:

```bash
# Generate SSH key pair
ssh-keygen -t rsa -b 4096 -f ~/.ssh/github_actions_blog -N "" -C "github-actions-blog-automation"

# Add public key to authorized_keys
cat ~/.ssh/github_actions_blog.pub >> ~/.ssh/authorized_keys
chmod 600 ~/.ssh/authorized_keys

# Display the private key (you'll need this for GitHub)
echo "=== PRIVATE KEY (copy this entire output) ==="
cat ~/.ssh/github_actions_blog
echo "=== END PRIVATE KEY ==="
```

**Copy the entire private key output** - you'll add this to GitHub secrets.

## Step 3: Find Your Network Information

```bash
# Get your local IP address
ifconfig | grep "inet " | grep -v 127.0.0.1 | awk '{print $2}' | head -n1

# Get your external IP address
curl -s https://ipecho.net/plain && echo
```

Write down both IP addresses.

## Step 4: Router Configuration (Port Forwarding)

You need to forward SSH traffic from the internet to your Mac:

1. **Find your router's admin panel:**
   - Usually `192.168.1.1` or `192.168.0.1`
   - Log in with admin credentials (often on router label)

2. **Set up port forwarding:**
   - Look for "Port Forwarding", "Virtual Server", or "NAT"
   - Create new rule:
     - **External Port:** 2222 (using non-standard port for security)
     - **Internal IP:** [Your Mac's local IP from Step 3]
     - **Internal Port:** 22
     - **Protocol:** TCP

3. **Save and restart router**

## Step 5: Test SSH Connection

From another network (like your phone's hotspot), test the connection:

```bash
# Test from external network
ssh -p 2222 -i ~/.ssh/github_actions_blog [YOUR_USERNAME]@[YOUR_EXTERNAL_IP]
```

If this works, you're ready for GitHub setup.

## Step 6: Set Up GitHub Repository Secrets

1. Go to your GitHub repository
2. Click **Settings** → **Secrets and variables** → **Actions**
3. Click **New repository secret** and add these:

**SSH_PRIVATE_KEY:**
```
[Paste the entire private key from Step 2]
```

**SSH_HOST:**
```
[Your external IP address]
```

**SSH_USER:**
```
[Your Mac username]
```

**SSH_PORT:**
```
2222
```

**GITHUB_TOKEN:**
1. Go to https://github.com/settings/tokens
2. Click **Generate new token (classic)**
3. Give it a name like "Blog Automation"
4. Check the **repo** scope
5. Copy the generated token and paste it as this secret

## Step 7: Update the Workflow File

The workflow file `.github/workflows/blog-automation.yml` is already created and should work with these settings.

## Step 8: Test the Setup

1. **Manual test:**
   - Go to your GitHub repository
   - Click **Actions** tab
   - Click **Nordic Numbers Blog Automation**
   - Click **Run workflow** → **Run workflow**

2. **Check logs:**
   - Wait for the workflow to complete
   - Click on the workflow run to see detailed logs
   - Look for any errors in the SSH connection or script execution

## Step 9: Security Considerations

1. **Change SSH port:** Using port 2222 instead of 22 reduces automated attacks
2. **Monitor access:** Check `/var/log/auth.log` occasionally for suspicious attempts
3. **Keep Mac updated:** Regular security updates are important
4. **Consider fail2ban:** Install fail2ban to block repeated failed attempts

## Step 10: Dynamic IP Solutions (Optional)

If your external IP changes frequently:

1. **Set up Dynamic DNS:**
   - Use services like DuckDNS, No-IP, or Dynu (free)
   - Update SSH_HOST secret to use your hostname instead of IP

2. **Router DDNS settings:**
   - Most routers have built-in DDNS support
   - Configure it to update your chosen service automatically

## Troubleshooting

**SSH connection fails:**
- Check port forwarding configuration
- Verify external IP hasn't changed
- Test SSH from local network first

**Workflow fails:**
- Check GitHub secrets are correct
- Verify SSH key permissions (should be 600)
- Look at workflow logs for specific error messages

**Scripts don't run:**
- Ensure scripts are executable: `chmod +x *.sh`
- Check file paths in scripts are correct
- Verify required dependencies are installed

## How It Works

1. GitHub Actions runs on GitHub's servers at midnight UTC
2. Connects to your Mac via SSH using the port forwarding
3. Executes your master_automation.sh script
4. Commits and pushes any changes back to GitHub
5. Your laptop can be closed/sleeping - it only needs to be awake and connected to internet

## Benefits

- ✅ Completely free (GitHub Actions gives 2000 minutes/month)
- ✅ Works with laptop closed
- ✅ Reliable scheduling
- ✅ Detailed logging
- ✅ Manual trigger option
- ✅ No monthly costs

This setup gives you professional-grade automation without any recurring costs!