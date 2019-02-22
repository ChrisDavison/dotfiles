$0~mon{
    tot += $2;
    NI += 1;
    cats[$4]++;
    cats_cost[$4] += $2
}
END{
    printf "%s : £%d (%d)\n", mon, tot, NI
    for (cat in cats) {
        printf "\t: %s : £%d (%d)\n", cat, cats_cost[cat], cats[cat]
    }
}
