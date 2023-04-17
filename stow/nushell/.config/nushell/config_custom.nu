# vim: noautoindent nosmartindent

# In env.nu, we should source this with:
#   NOTE(Chris): We use `~` because it's apparently a constant and `source` requires a constant
#   source ~\dotfiles\stow\nushell\.config\nushell\nu_custom.nu

let custom_config = {
  show_banner: false
  # TODO(Chris): Implement some kind of comprehensive merge, so that this is less janky
  keybindings: ($env.config.keybindings | append [
    {
      name: lf
      modifier: control
      keycode: char_w
      mode: [emacs, vi_normal, vi_insert]
      event: {
        send: executehostcommand
       cmd: "lfcd"
      }
    }
  ])
}

let-env config = ($env.config | merge $custom_config)

def-env lfcd [] {
  lf -last-dir-path ~/tmp/lfcd_last_dir.txt
  cd (open --raw ~/tmp/lfcd_last_dir.txt)
}

def mvdo [minutes_ago: int] {
  let hours_ago_duration = ($"($minutes_ago)min" | into duration)

  let recent_downloads = (ls -a ~\Downloads\ | where { |file|
    let duration = (date now) - $file.modified
    $duration < $hours_ago_duration
  })

  if ($recent_downloads | length) <= 0 {
    echo 'No downloads were recent enough, sorry.'
  } else {
    $recent_downloads | each { |file_info|
      mv ($file_info.name) .
      $file_info
    }
  }
}

alias l = ls -a
alias cdd = cd ~/dotfiles
alias e = nvim
alias lg = lazygit
alias ln = gsudo nu ~/bin/ln.nu

# Set up zoxide

if ('~/.zoxide.nu' | path exists) {
  source ~/.zoxide.nu
}
