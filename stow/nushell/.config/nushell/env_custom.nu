# In env.nu, we should source this with:
#   NOTE(Chris): We use `~` because it's apparently a constant and `source` requires a constant
#   source ~\dotfiles\stow\nushell\.config\nushell\env_custom.nu

let-env Path = ($env.Path | split row (char esep) | prepend ($"($env.HOMEPATH)/bin"))

let-env EDITOR = 'nvim'

# Set up zoxide

if (which zoxide | length) > 0 { 
  zoxide init nushell --cmd j | save -f ~/.zoxide.nu
}
