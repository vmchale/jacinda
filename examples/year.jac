{. extract year from column of dates in YYYY-MM-DD format
.? {|`1 ~* 1 /(\d{4})-\d{2}-\d{2}/}
