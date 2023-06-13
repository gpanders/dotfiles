function weechat
    # Set $HOSTNAME to use in username
    set -gx HOSTNAME (hostname)

    # Needed to update scripts list on macOS (https://weechat.org/files/doc/devel/weechat_faq.en.html#scripts_update)
    set -gx OBJC_DISABLE_INITIALIZE_FORK_SAFETY YES

    command weechat $argv
end
