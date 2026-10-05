# New Mac Playbook

_Written 2026-10-05 while migrating off the company laptop. Companion to
`~/Documents/MIGRATION-CHECKLIST.md` (which lives in the restic backup)._

## Phase 0 — before handing back the old laptop

- [ ] Final `~/code/dotfiles/config/restic/backup.sh` run; verify with
      `restic snapshots` and `restic check`
- [ ] Finish checklist reviews (org files, ekg notes, Downloads PDFs,
      recovery_codes currency)
- [ ] Push any last personal git work
- [ ] KEEP THE OLD LAPTOP until the new one is fully verified

## Phase 1 — bootstrap (order matters)

### 1. Identity & access

- Sign into iCloud (Photos/Keychain start syncing)
- Generate a NEW ssh key: `ssh-keygen -t ed25519`
- Add pubkey to github.com/AnselmC (web UI) and to the Hetzner storage box
  (Robot → Storage Box → SSH keys, or `ssh-copy-id -p 23 u684523@u684523.your-storagebox.de`
  using the box password from your password manager)

### 2. Tooling

```bash
xcode-select --install
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
git clone git@github.com:AnselmC/dotfiles.git ~/code/dotfiles
brew bundle install --file ~/code/dotfiles/Brewfile   # includes restic, direnv, enchant, emacs-plus…
```

### 3. Dotfiles symlinks

```bash
ln -s ~/code/dotfiles/.emacs ~/.emacs
ln -s ~/code/dotfiles/early-init.el ~/.emacs.d/early-init.el   # mkdir ~/.emacs.d first
ln -s ~/code/dotfiles/.zshrc ~/.zshrc   # or merge if drifted
ln -s ~/code/dotfiles/.vimrc ~/.vimrc
mkdir -p ~/.config/direnv && ln -s ~/code/dotfiles/config/direnv/direnvrc ~/.config/direnv/direnvrc
~/code/dotfiles/pi/install.sh           # pi agent config/extensions/presets
```

### 4. Restore personal data from restic

```bash
export RESTIC_REPOSITORY="sftp:storagebox:restic-home"
export RESTIC_PASSWORD   # from password manager (or RESTIC_PASSWORD_FILE after step 5)
restic snapshots         # sanity check
# Selective restore — do NOT blanket-restore old ~/Library over a fresh system:
restic restore latest --target / \
  --include ~/Documents --include ~/org \
  --include ~/Pictures --include ~/Music --include ~/Movies \
  --include ~/Zotero --include ~/Desktop \
  --include ~/.kalshi --include ~/.secrets --include ~/.authinfo \
  --include ~/.pi/agent/sessions
# (paths assume same username `anselm`; otherwise restore to a staging
#  dir with --target ~/restored and move things into place)
```

### 5. Secrets hygiene

- `chmod 600 ~/.secrets/* ~/.kalshi/* ~/.authinfo`
- Recreate `~/.secrets/RESTIC_PASSWORD` from the password manager

### 6. Dev runtimes

```bash
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/master/install.sh | bash
nvm install 24.13.0 && nvm install 24.4.1 && nvm alias default 24.4.1
npm i -g @earendil-works/pi-coding-agent
pi   # login to Anthropic etc. on first run
# direnv hook comes with .zshrc; per-project .envrc/.nvmrc restore via git/restic
```

### 7. Emacs

- First launch: straight.el bootstraps and builds every package (10–20 min).
- tree-sitter grammars: treesit-auto will prompt per language.
- ekg notes: restore `~/.emacs.d/triples.db` from restic if you kept it:
  `restic restore latest --target / --include ~/.emacs.d/triples.db`
- Clone your pimacs fork if you want local dev:
  straight will otherwise fetch ananthakumaran/pimacs.el per .emacs recipe.

### 8. Re-enable backups on the NEW machine (same repo)

```bash
# paths in backup.sh/excludes/plist assume /Users/anselm — adjust if different
cp ~/code/dotfiles/config/restic/me.restic-backup.plist ~/Library/LaunchAgents/
launchctl load ~/Library/LaunchAgents/me.restic-backup.plist
```

Snapshots are per-hostname; the repo handles multiple machines fine.

## Phase 2 — verification (run the new machine ≥ a few days)

- [ ] Kalshi key works (pm-quant against prod)
- [ ] Email (authinfo), Zotero sync, browser profile, 2FA apps
- [ ] Photos library opened and complete
- [ ] pi + pimacs: resume an old session
- [ ] restic: new snapshot from new Mac appears; `restic check` clean

## Phase 3 — decommission old laptop

- [ ] Revoke OLD laptop's ssh key from GitHub + storage box
- [ ] Rotate ANTHROPICKEY / OPENAIKEY (they lived on company hardware)
- [ ] Work through the "Leave behind / wipe" section of MIGRATION-CHECKLIST.md
- [ ] Sign out iCloud, FileVault-erase personal dirs, hand back
