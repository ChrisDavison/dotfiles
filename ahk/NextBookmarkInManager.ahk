nextBookmarkInManager(delay){
    Send +{Tab}
    sleep, delay
    Send {Down}
    sleep, delay
    Send {Tab}
    sleep, delay
    Send {Home}
    sleep, delay
    Send ^{Right}^{Right}
    sleep, delay
    Send {Left}{Left}
    sleep, delay
    Send, +{End}
    sleep, delay
    Send, {Del}
}
