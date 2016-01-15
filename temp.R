injuries_by_year <-
  df %>%
  group_by( year = year(BGN_DATE)) %>%
  summarise(
    total_fatalities = sum(FATALITIES),
    total_injuries   = sum(INJURIES),
    total_propdmg    = sum(PROPDMG),
    total_cropdmg    = sum(CROPDMG)
    )

df$evtype_match <- amatch(df$EVTYPE, evtypes, method='soundex', maxDist = 0.3)



df[is.na(df$evtype_match) == TRUE,] %>% group_by(EVTYPE) %>% summarise(count = sum(is.na(evtype_match))) %>% arrange(desc(count))

## Damage plot
dmg_by_evtype <-
  df %>%
  group_by(EVTYPE) %>%
  summarise(
    total_fatalities = sum(FATALITIES),
    total_injuries   = sum(INJURIES),
    total_propdmg    = sum(PROPDMG),
    total_cropdmg    = sum(CROPDMG)
  ) %>%
  arrange(desc(total_propdmg+total_cropdmg))

# lattice rearrange labels in alphabetical order - this is
# a way enforce ordering

top_five_by_total_dmg <- head(dmg_by_evtype, 5)
x_labels <- factor(top_five_by_total_dmg$EVTYPE, levels = unique(top_five_by_total_dmg$EVTYPE))
barchart(total_propdmg+total_cropdmg~x_labels, data=top_five_by_total_dmg, stack = TRUE)

## Health plot
# dmg_by_evtype <-
#   df %>%
#   group_by(EVTYPE) %>%
#   summarise(
#     total_fatalities = sum(FATALITIES),
#     total_injuries   = sum(INJURIES),
#     total_health        = sum(FATALITIES+INJURIES)
#   ) %>%
#   arrange(desc(total_health)) %>%
#   head(10)

# arrange(dmg_by_evtype, desc(total_fatalities+total_injuries))

top_five_by_total_health <- head(arrange(dmg_by_evtype, desc(total_fatalities+total_injuries)), 5)

x_labels <- factor(top_five_by_total_health$EVTYPE, levels = unique(top_five_by_total_health$EVTYPE))

barchart( total_fatalities+total_injuries~x_labels,
          data=top_five_by_total_health,
          stack = TRUE,
          auto.legend = TRUE,
          auto.key=list(
            space="top",
            columns=4,
            title="Health threats",
            cex.title=1
          )
        )




