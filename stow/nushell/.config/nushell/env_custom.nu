# In env.nu, we should source this with:
#   source ($nu.default-config-dir | path join 'env_custom.nu')

$env.Path = ($env.Path | split row (char esep) | prepend ($"($env.HOMEPATH)/bin"))

$env.EDITOR = 'nvim'

# Set up zoxide

if (which zoxide | length) > 0 { 
  zoxide init nushell --cmd j | save -f ~/.zoxide.nu
}
