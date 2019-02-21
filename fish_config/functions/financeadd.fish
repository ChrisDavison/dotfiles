function financeadd
    if [ -z $FINANCEFILE ]
        echo "Need to define FINANCEFILE"
        return 1
    end
    read -P "Date: " ddate
    read -P "Cost: " cost
    read -P "Description: " desc
    read -P "Category: " category
    read -P "Comment: " comment
    echo $ddate","$cost","$desc","$category","$comment >> $FINANCEFILE
end
