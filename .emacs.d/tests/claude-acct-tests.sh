#!/usr/bin/env bash
#
# claude-acct-tests.sh — behaviour of bin/claude-acct.
#
# The wrapper decides which account a session runs under, and every way it can
# be wrong is quiet: the session simply runs as somebody else. Stub `claude'
# and `op' so the token precedence can be exercised without a real session or a
# real secret.

set -uo pipefail

here="$(cd "$(dirname -- "$0")" && pwd -P)"
script="$here/../../bin/claude-acct"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# The wrapper is configured entirely by environment, so the developer running
# these tests is very likely to have exactly the variables under test already
# exported -- that setup is what bin/claude-acct documents. An inherited
# CLAUDE_ACCT_TOKEN_WORK satisfies the direct-token branch before the 1Password
# cases can reach it, and two assertions pass for the wrong reason. Start from a
# clean slate so the only values in play are the ones each case sets.
while read -r var; do
  case "$var" in
    CLAUDE_ACCT_*|ANTHROPIC_*) unset "$var" ;;
  esac
done < <(compgen -v)

pass=0; fail=0
ok()   { pass=$((pass+1)); printf '  ok     %s\n' "$1"; }
bad()  { fail=$((fail+1)); printf '  FAIL   %s\n     expected: %s\n     actual:   %s\n' "$1" "$2" "$3"; }
check(){ if [ "$2" = "$3" ]; then ok "$1"; else bad "$1" "$2" "$3"; fi; }
contains(){ case "$3" in *"$2"*) ok "$1";; *) bad "$1" "contains: $2" "$3";; esac; }

mkdir -p "$work/bin"
cat > "$work/bin/claude" <<'STUB'
#!/usr/bin/env bash
printf 'argv=%s token=%s apikey=%s authtoken=%s\n' "$*" "${CLAUDE_CODE_OAUTH_TOKEN:-<unset>}" "${ANTHROPIC_API_KEY:-<unset>}" "${ANTHROPIC_AUTH_TOKEN:-<unset>}"
STUB
cat > "$work/bin/op" <<'STUB'
#!/usr/bin/env bash
[ "$1" = read ] && printf 'TOKEN-FROM-1PASSWORD\n'
STUB
chmod +x "$work/bin/claude" "$work/bin/op"
export CLAUDE_ACCT_CLAUDE_BIN="$work/bin/claude"

# Both binaries are named explicitly, never found on PATH. Narrowing PATH to
# hide `op' does not work portably -- Homebrew puts it outside /usr/bin:/bin
# and apt puts it inside -- so a PATH-based test passes on macOS, fails on
# Linux, and on the way there runs `op read' against the developer's real,
# signed-in 1Password account. No test here may reach a real secret store.
export CLAUDE_ACCT_OP_BIN="$work/bin/op"
absent_op="$work/bin/no-such-op"

echo "claude-acct"

# A stray API key outranks both the token and the Keychain login, silently
# changing how the session is billed — so it must be cleared for every account.
out=$(PATH="$work/bin:$PATH" ANTHROPIC_API_KEY=sk-leak ANTHROPIC_AUTH_TOKEN=sk-leak2 \
      CLAUDE_ACCT_AMBIENT_ME=1 "$script" me 2>&1)
contains "clears ANTHROPIC_API_KEY on an ambient account"    "apikey=<unset>"    "$out"
contains "clears ANTHROPIC_AUTH_TOKEN on an ambient account" "authtoken=<unset>" "$out"

out=$(PATH="$work/bin:$PATH" ANTHROPIC_API_KEY=sk-leak ANTHROPIC_AUTH_TOKEN=sk-leak2 \
      CLAUDE_ACCT_TOKEN_WORK=DIRECT "$script" work 2>&1)
contains "clears ANTHROPIC_API_KEY on a token account"       "apikey=<unset>"    "$out"
contains "clears ANTHROPIC_AUTH_TOKEN on a token account"    "authtoken=<unset>" "$out"
contains "direct token is used"                              "token=DIRECT"      "$out"

# op is only for the _REF form, so a machine without it still works. Proven by
# pointing CLAUDE_ACCT_OP_BIN at nothing: if the direct token path consulted op
# at all, this would report it missing instead of running.
out=$(CLAUDE_ACCT_OP_BIN="$absent_op" CLAUDE_ACCT_TOKEN_WORK=DIRECT "$script" work 2>&1)
contains "direct token needs no op at all" "token=DIRECT" "$out"

out=$(CLAUDE_ACCT_TOKEN_REF_WORK="op://V/i/c" "$script" work 2>&1)
contains "falls back to the 1Password reference" "token=TOKEN-FROM-1PASSWORD" "$out"

out=$(CLAUDE_ACCT_TOKEN_WORK=DIRECT CLAUDE_ACCT_TOKEN_REF_WORK="op://V/i/c" "$script" work 2>&1)
contains "direct token wins over the reference" "token=DIRECT" "$out"

out=$(CLAUDE_ACCT_OP_BIN="$absent_op" CLAUDE_ACCT_TOKEN_REF_WORK="op://V/i/c" "$script" work 2>&1)
contains "missing op is reported, not ignored" "1Password CLI not found" "$out"
contains "missing op names the path it tried" "$absent_op"                "$out"

# A failing op must stop the session rather than fall through to some other
# identity: an expired or locked 1Password is the common case.
cat > "$work/bin/op-fails" <<'STUB'
#!/usr/bin/env bash
echo "[ERROR] could not read secret" >&2
exit 1
STUB
chmod +x "$work/bin/op-fails"
out=$(CLAUDE_ACCT_OP_BIN="$work/bin/op-fails" CLAUDE_ACCT_TOKEN_REF_WORK="op://V/i/c" "$script" work 2>&1)
contains "a failing op is fatal, not a fallback" "could not read the token" "$out"
case "$out" in *"argv="*) bad "a failing op never reaches claude" "no claude run" "$out";; *) ok "a failing op never reaches claude";; esac

# An empty token is a silent wrong-account start if it slips through.
cat > "$work/bin/op-empty" <<'STUB'
#!/usr/bin/env bash
printf ''
STUB
chmod +x "$work/bin/op-empty"
out=$(CLAUDE_ACCT_OP_BIN="$work/bin/op-empty" CLAUDE_ACCT_TOKEN_REF_WORK="op://V/i/c" "$script" work 2>&1)
contains "an empty token from op is rejected" "empty token" "$out"

# An account with nothing configured is legitimate, but so is a typo, and they
# look identical — so it says so rather than quietly using the wrong identity.
out=$(PATH="$work/bin:$PATH" "$script" typo 2>&1)
contains "unconfigured account reports the fallback" "using the ambient login" "$out"
out=$(PATH="$work/bin:$PATH" CLAUDE_ACCT_AMBIENT_TYPO=1 "$script" typo 2>&1)
case "$out" in *"using the ambient login"*) bad "declared ambient is quiet" "no notice" "$out";; *) ok "declared ambient is quiet";; esac

# An option must never be taken for an account: doing so ran claude with no
# arguments at all, under the wrong identity.
out=$(PATH="$work/bin:$PATH" "$script" --resume 2>&1)
contains "an option is not an account name" "expected an account name" "$out"
out=$(PATH="$work/bin:$PATH" "$script" -c 2>&1)
contains "a short option is not an account name" "expected an account name" "$out"

out=$(PATH="$work/bin:$PATH" CLAUDE_ACCT_TOKEN_WORK=DIRECT "$script" work --resume -p hello 2>&1)
contains "arguments after the account pass through" "argv=--resume -p hello" "$out"

out=$(PATH="$work/bin:$PATH" "$script" 'evil;name' 2>&1)
contains "invalid account name is rejected" "invalid account name" "$out"

out=$(PATH="$work/bin:$PATH" "$script" < /dev/null 2>&1)
contains "no account and no terminal is an error" "no account given" "$out"

out=$(PATH="$work/bin:$PATH" CLAUDE_ACCT_CLAUDE_BIN="$script" "$script" work 2>&1)
contains "self-reference is caught" "points back at this script" "$out"

# A CLAUDE_ACCT_CLAUDE_BIN pointing at nothing must name the path it tried,
# since the usual cause is a claude installed somewhere other than the default.
out=$(PATH="$work/bin:$PATH" CLAUDE_ACCT_CLAUDE_BIN="$work/bin/absent" "$script" work 2>&1)
contains "a missing claude binary names the path" "$work/bin/absent" "$out"

# dashes fold to underscores in the variable name
out=$(PATH="$work/bin:$PATH" CLAUDE_ACCT_TOKEN_MY_ORG=DASHED "$script" my-org 2>&1)
contains "dashes map to underscores in the lookup" "token=DASHED" "$out"

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
