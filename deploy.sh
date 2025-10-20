#!/bin/bash
echo "📦 Deploying DKA Calculator..."

git add .
git commit -m "Update: $(date '+%Y-%m-%d %H:%M')"
git push origin main

ssh dsteele@uelgrande "cd /srv/shiny-server/dka && sudo git pull origin main"

echo "✅ Deployed to uelgrande and backed up on GitHub!"