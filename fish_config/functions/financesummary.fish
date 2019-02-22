function financesummary
    set -l thismonth (date +"%Y-%m")
    set -l lastmonth (date -v -1m +"%Y-%m")
    set -l 2monthsago (date -v -2m +"%Y-%m")

    awk -F',' -v mon=$thismonth -f $HOME/code/dotfiles/finance_summary.awk $FINANCEFILE | column -s':' -t
    string repeat -n 48 "="
    awk -F',' -v mon=$lastmonth -f $HOME/code/dotfiles/finance_summary.awk $FINANCEFILE | column -s':' -t
    string repeat -n 48 "="
    awk -F',' -v mon=$2monthsago -f $HOME/code/dotfiles/finance_summary.awk $FINANCEFILE | column -s':' -t
    string repeat -n 48 "="
    awk -F',' '
        NR==1{next}
        {
            cat_cost[$4] += $2;
            num_cat[$4]++
        } 
        END{
            print "COST PER CATEGORY"
            for(cat in cat_cost){
                printf "\t%s (#%d) : £%d : (avg £%d)\n", cat, num_cat[cat], cat_cost[cat], cat_cost[cat] / num_cat[cat]
            }
        }' $FINANCEFILE | column -s':' -t
end

