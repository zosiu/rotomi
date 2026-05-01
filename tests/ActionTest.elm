module ActionTest exposing (suite)

import Action exposing (..)
import Expect
import Replay exposing (ReplayLine(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Action.parseAction"
        [ describe "KnockedOut"
            [ test "parses knocked out line" <|
                \_ ->
                    parseAction "NoxFoxEX's (svbsp_149) Pecharunt was Knocked Out!"
                        |> Expect.equal
                            (KnockedOut
                                { pokemon = { player = "NoxFoxEX", card = { id = "svbsp_149", name = "Pecharunt" } }
                                }
                            )
            , test "parses knocked out with multi-word name" <|
                \_ ->
                    parseAction "zosiu's (me1_163) Mega Latias ex was Knocked Out!"
                        |> Expect.equal
                            (KnockedOut
                                { pokemon = { player = "zosiu", card = { id = "me1_163", name = "Mega Latias ex" } }
                                }
                            )
            ]
        , describe "TurnEnded"
            [ test "parses end of turn" <|
                \_ ->
                    parseAction "zosiu ended their turn."
                        |> Expect.equal (TurnEnded { player = "zosiu" })
            , test "parses end of turn for opponent" <|
                \_ ->
                    parseAction "NoxFoxEX ended their turn."
                        |> Expect.equal (TurnEnded { player = "NoxFoxEX" })
            ]
        , describe "UsedAttack"
            [ test "parses attack with target and damage" <|
                \_ ->
                    parseAction "NoxFoxEX's (me1_163) Mega Latias ex used Strafe on zosiu's (me2_56) Mega Gengar ex for 80 damage."
                        |> Expect.equal
                            (UsedAttack
                                { attacker = { player = "NoxFoxEX", card = { id = "me1_163", name = "Mega Latias ex" } }
                                , move = "Strafe"
                                , target =
                                    Just
                                        { defender = { player = "zosiu", card = { id = "me2_56", name = "Mega Gengar ex" } }
                                        , damage = 80
                                        }
                                , modifier = Nothing
                                }
                            )
            , test "parses attack without target" <|
                \_ ->
                    parseAction "NoxFoxEX's (sv2_247) Squawkabilly ex used Squawk and Seize."
                        |> Expect.equal
                            (UsedAttack
                                { attacker = { player = "NoxFoxEX", card = { id = "sv2_247", name = "Squawkabilly ex" } }
                                , move = "Squawk and Seize."
                                , target = Nothing
                                , modifier = Nothing
                                }
                            )
            , test "parses attack with Weakness modifier" <|
                \_ ->
                    parseAction "zosiu's (sv10_136) Marnie's Grimmsnarl ex used Shadow Bullet on Master_Kurt55's (sv6-5_72) Munkidori for 360 damage. Master_Kurt55's (sv6-5_72) Munkidori took 180 more damage because of Darkness Weakness."
                        |> Expect.equal
                            (UsedAttack
                                { attacker = { player = "zosiu", card = { id = "sv10_136", name = "Marnie's Grimmsnarl ex" } }
                                , move = "Shadow Bullet"
                                , target =
                                    Just
                                        { defender = { player = "Master_Kurt55", card = { id = "sv6-5_72", name = "Munkidori" } }
                                        , damage = 360
                                        }
                                , modifier = Just (Weakness "Darkness" 180)
                                }
                            )
            , test "parses attack with Resistance modifier" <|
                \_ ->
                    parseAction "zosiu's (sv6_110_ph) Ting-Lu used Ground Crasher on The_Gifted95's (me1_56) Alakazam for 0 damage. The_Gifted95's (me1_56) Alakazam took -30 less damage because of Fighting Resistance."
                        |> Expect.equal
                            (UsedAttack
                                { attacker = { player = "zosiu", card = { id = "sv6_110_ph", name = "Ting-Lu" } }
                                , move = "Ground Crasher"
                                , target =
                                    Just
                                        { defender = { player = "The_Gifted95", card = { id = "me1_56", name = "Alakazam" } }
                                        , damage = 0
                                        }
                                , modifier = Just (Resistance "Fighting" 30)
                                }
                            )
            ]
        , describe "PlayedPokemon"
            [ test "parses played to Active Spot" <|
                \_ ->
                    parseAction "zosiu played (sv6-5_36) Okidogi ex to the Active Spot."
                        |> Expect.equal
                            (PlayedPokemon
                                { player = "zosiu"
                                , card = { id = "sv6-5_36", name = "Okidogi ex" }
                                , position = ActiveSpot
                                }
                            )
            , test "parses played to Bench" <|
                \_ ->
                    parseAction "NoxFoxEX played (svbsp_149) Pecharunt to the Bench."
                        |> Expect.equal
                            (PlayedPokemon
                                { player = "NoxFoxEX"
                                , card = { id = "svbsp_149", name = "Pecharunt" }
                                , position = BenchSpot
                                }
                            )
            ]
        , describe "PlayedStadium"
            [ test "parses played to Stadium spot" <|
                \_ ->
                    parseAction "zosiu played (sv6-5_54_ph) Academy at Night to the Stadium spot."
                        |> Expect.equal
                            (PlayedStadium
                                { player = "zosiu"
                                , card = { id = "sv6-5_54_ph", name = "Academy at Night" }
                                }
                            )
            ]
        , describe "UsedStadium"
            [ test "parses stadium re-use without card ID" <|
                \_ ->
                    parseAction "zosiu played Academy at Night."
                        |> Expect.equal (UsedStadium { player = "zosiu", name = "Academy at Night" })
            ]
        , describe "PlayedTrainer"
            [ test "parses played supporter" <|
                \_ ->
                    parseAction "NoxFoxEX played (sv1_166) Arven."
                        |> Expect.equal
                            (PlayedTrainer
                                { player = "NoxFoxEX"
                                , card = { id = "sv1_166", name = "Arven" }
                                }
                            )
            , test "parses played item" <|
                \_ ->
                    parseAction "NoxFoxEX played (sv1_181_ph) Nest Ball."
                        |> Expect.equal
                            (PlayedTrainer
                                { player = "NoxFoxEX"
                                , card = { id = "sv1_181_ph", name = "Nest Ball" }
                                }
                            )
            ]
        , describe "Drew"
            [ test "parses drew a card (private)" <|
                \_ ->
                    parseAction "NoxFoxEX drew a card."
                        |> Expect.equal (Drew { player = "NoxFoxEX", card = Nothing })
            , test "parses drew a named card" <|
                \_ ->
                    parseAction "zosiu drew (sv6-5_72) Munkidori."
                        |> Expect.equal
                            (Drew
                                { player = "zosiu"
                                , card = Just { id = "sv6-5_72", name = "Munkidori" }
                                }
                            )
            ]
        , describe "DrewCard"
            [ test "parses detail drew card and played to Bench" <|
                \_ ->
                    parseAction "NoxFoxEX drew (sv4_123) Brute Bonnet and played it to the Bench."
                        |> Expect.equal
                            (DrewCard
                                { player = "NoxFoxEX"
                                , card = { id = "sv4_123", name = "Brute Bonnet" }
                                , andPlayed = Just BenchSpot
                                }
                            )
            , test "drew named card without play is parsed as Drew" <|
                \_ ->
                    parseAction "NoxFoxEX drew (sv1_166) Arven."
                        |> Expect.equal
                            (Drew
                                { player = "NoxFoxEX"
                                , card = Just { id = "sv1_166", name = "Arven" }
                                }
                            )
            ]
        , describe "Attached"
            [ test "parses attachment to Bench" <|
                \_ ->
                    parseAction "NoxFoxEX attached (sve_13_ph) Basic Psychic Energy to (me1_163) Mega Latias ex on the Bench."
                        |> Expect.equal
                            (Attached
                                { player = "NoxFoxEX"
                                , item = { id = "sve_13_ph", name = "Basic Psychic Energy" }
                                , target = { player = "NoxFoxEX", card = { id = "me1_163", name = "Mega Latias ex" } }
                                , position = BenchSpot
                                }
                            )
            , test "parses attachment to Active Spot" <|
                \_ ->
                    parseAction "Alannvs86 attached (mee_1) Basic Grass Energy to (sv10_21) Smoliv in the Active Spot."
                        |> Expect.equal
                            (Attached
                                { player = "Alannvs86"
                                , item = { id = "mee_1", name = "Basic Grass Energy" }
                                , target = { player = "Alannvs86", card = { id = "sv10_21", name = "Smoliv" } }
                                , position = ActiveSpot
                                }
                            )
            ]
        , describe "Evolved"
            [ test "parses evolution on Bench" <|
                \_ ->
                    parseAction "The_Gifted95 evolved (sv2_156) Dunsparce to (sv5_129) Dudunsparce on the Bench."
                        |> Expect.equal
                            (Evolved
                                { player = "The_Gifted95"
                                , from = { id = "sv2_156", name = "Dunsparce" }
                                , to = { id = "sv5_129", name = "Dudunsparce" }
                                , position = BenchSpot
                                }
                            )
            , test "parses evolution in Active Spot" <|
                \_ ->
                    parseAction "Alannvs86 evolved (sv10_21) Smoliv to (sv10_22) Dolliv in the Active Spot."
                        |> Expect.equal
                            (Evolved
                                { player = "Alannvs86"
                                , from = { id = "sv10_21", name = "Smoliv" }
                                , to = { id = "sv10_22", name = "Dolliv" }
                                , position = ActiveSpot
                                }
                            )
            ]
        , describe "MovedToActive"
            [ test "parses moved to Active Spot" <|
                \_ ->
                    parseAction "NoxFoxEX's (svbsp_149) Pecharunt is now in the Active Spot."
                        |> Expect.equal
                            (MovedToActive
                                { pokemon = { player = "NoxFoxEX", card = { id = "svbsp_149", name = "Pecharunt" } }
                                }
                            )
            ]
        , describe "Retreated"
            [ test "parses retreat to Bench" <|
                \_ ->
                    parseAction "NoxFoxEX retreated (svbsp_149) Pecharunt to the Bench."
                        |> Expect.equal
                            (Retreated
                                { player = "NoxFoxEX"
                                , card = { id = "svbsp_149", name = "Pecharunt" }
                                }
                            )
            ]
        , describe "ConditionApplied"
            [ test "parses poisoned" <|
                \_ ->
                    parseAction "NoxFoxEX's (svbsp_149) Pecharunt is now Poisoned."
                        |> Expect.equal
                            (ConditionApplied
                                { pokemon = { player = "NoxFoxEX", card = { id = "svbsp_149", name = "Pecharunt" } }
                                , condition = "Poisoned"
                                }
                            )
            ]
        , describe "ConditionRemoved"
            [ test "parses condition removed" <|
                \_ ->
                    parseAction "zosiu's (sv6-5_36) Okidogi ex is no longer Poisoned."
                        |> Expect.equal
                            (ConditionRemoved
                                { pokemon = { player = "zosiu", card = { id = "sv6-5_36", name = "Okidogi ex" } }
                                , condition = "Poisoned"
                                }
                            )
            ]
        , describe "TookDamage"
            [ test "parses took damage" <|
                \_ ->
                    parseAction "zosiu's (sv8-5_36_sph) Dusclops took 90 damage."
                        |> Expect.equal
                            (TookDamage
                                { pokemon = { player = "zosiu", card = { id = "sv8-5_36_sph", name = "Dusclops" } }
                                , amount = 90
                                }
                            )
            ]
        , describe "CardActivated"
            [ test "parses card activated" <|
                \_ ->
                    parseAction "(sv4_159) Ancient Booster Energy Capsule was activated."
                        |> Expect.equal
                            (CardActivated { card = { id = "sv4_159", name = "Ancient Booster Energy Capsule" } })
            , test "parses energy activated" <|
                \_ ->
                    parseAction "(sv2_190) Jet Energy was activated."
                        |> Expect.equal
                            (CardActivated { card = { id = "sv2_190", name = "Jet Energy" } })
            ]
        , describe "CardDiscardedFrom"
            [ test "parses card discarded from pokemon" <|
                \_ ->
                    parseAction "(sv4_159) Ancient Booster Energy Capsule was discarded from NoxFoxEX's (sv4_123) Brute Bonnet."
                        |> Expect.equal
                            (CardDiscardedFrom
                                { card = { id = "sv4_159", name = "Ancient Booster Energy Capsule" }
                                , pokemon = { player = "NoxFoxEX", card = { id = "sv4_123", name = "Brute Bonnet" } }
                                }
                            )
            ]
        , describe "CardAddedToHand"
            [ test "parses named card added to hand" <|
                \_ ->
                    parseAction "(sv6-5_61) Night Stretcher was added to zosiu's hand."
                        |> Expect.equal
                            (CardAddedToHand
                                { card = Just { id = "sv6-5_61", name = "Night Stretcher" }
                                , player = "zosiu"
                                }
                            )
            , test "parses hidden card added to hand" <|
                \_ ->
                    parseAction "A card was added to NoxFoxEX's hand."
                        |> Expect.equal (CardAddedToHand { card = Nothing, player = "NoxFoxEX" })
            ]
        , describe "OpeningDraw"
            [ test "parses 7-card opening draw" <|
                \_ ->
                    parseAction "zosiu drew 7 cards for the opening hand."
                        |> Expect.equal (OpeningDraw { player = "zosiu", count = 7 })
            ]
        , describe "CoinFlipChoice"
            [ test "parses tails choice" <|
                \_ ->
                    parseAction "zosiu chose tails for the opening coin flip."
                        |> Expect.equal (CoinFlipChoice { player = "zosiu", choice = "tails" })
            , test "parses heads choice" <|
                \_ ->
                    parseAction "ManuelZ1702 chose heads for the opening coin flip."
                        |> Expect.equal (CoinFlipChoice { player = "ManuelZ1702", choice = "heads" })
            ]
        , describe "CoinTossWon"
            [ test "parses coin toss won" <|
                \_ ->
                    parseAction "NoxFoxEX won the coin toss."
                        |> Expect.equal (CoinTossWon { player = "NoxFoxEX" })
            ]
        , describe "GoDecision"
            [ test "parses decided to go second" <|
                \_ ->
                    parseAction "NoxFoxEX decided to go second."
                        |> Expect.equal (GoDecision { player = "NoxFoxEX", first = False })
            , test "parses decided to go first" <|
                \_ ->
                    parseAction "zosiu decided to go first."
                        |> Expect.equal (GoDecision { player = "zosiu", first = True })
            ]
        , describe "MulliganTaken"
            [ test "parses single mulligan" <|
                \_ ->
                    parseAction "zosiu took a mulligan."
                        |> Expect.equal (MulliganTaken { player = "zosiu", count = 1 })
            , test "parses two mulligans" <|
                \_ ->
                    parseAction "zosiu took 2 mulligans."
                        |> Expect.equal (MulliganTaken { player = "zosiu", count = 2 })
            ]
        , describe "MulliganBonus"
            [ test "parses mulligan bonus draw" <|
                \_ ->
                    parseAction "NoxFoxEX drew 2 more cards because zosiu took at least 1 mulligan."
                        |> Expect.equal
                            (MulliganBonus
                                { player = "NoxFoxEX"
                                , count = 2
                                , because = "zosiu took at least 1 mulligan"
                                }
                            )
            ]
        , describe "TookPrize"
            [ test "parses single prize" <|
                \_ ->
                    parseAction "zosiu took a Prize card."
                        |> Expect.equal (TookPrize { player = "zosiu", count = 1 })
            , test "parses multiple prizes" <|
                \_ ->
                    parseAction "zosiu took 3 Prize cards."
                        |> Expect.equal (TookPrize { player = "zosiu", count = 3 })
            ]
        , describe "PoisonCheckupDamage"
            [ test "parses single counter from poison" <|
                \_ ->
                    parseAction "1 damage counter was placed on NoxFoxEX's (svbsp_149) Pecharunt for the Special Condition Poisoned."
                        |> Expect.equal
                            (PoisonCheckupDamage
                                { pokemon = { player = "NoxFoxEX", card = { id = "svbsp_149", name = "Pecharunt" } }
                                , counters = 1
                                }
                            )
            , test "parses multiple counters from poison" <|
                \_ ->
                    parseAction "6 damage counters were placed on zosiu's (sv6-5_36) Okidogi ex for the Special Condition Poisoned."
                        |> Expect.equal
                            (PoisonCheckupDamage
                                { pokemon = { player = "zosiu", card = { id = "sv6-5_36", name = "Okidogi ex" } }
                                , counters = 6
                                }
                            )
            ]
        , describe "DamagePrevented"
            [ test "parses damage prevented" <|
                \_ ->
                    parseAction "Damage to (sv2_156) Dunsparce was prevented."
                        |> Expect.equal
                            (DamagePrevented { pokemon = { id = "sv2_156", name = "Dunsparce" } })
            ]
        , describe "EffectBlocked"
            [ test "parses effect blocked" <|
                \_ ->
                    parseAction "Effects of Phantom Dive did not affect (me1_164) Mega Kangaskhan ex."
                        |> Expect.equal
                            (EffectBlocked
                                { move = "Phantom Dive"
                                , pokemon = { id = "me1_164", name = "Mega Kangaskhan ex" }
                                }
                            )
            ]
        , describe "SpecialConditionImmune"
            [ test "parses special condition immunity" <|
                \_ ->
                    parseAction "(sv4_123) Brute Bonnet cannot be affected by Special Conditions."
                        |> Expect.equal
                            (SpecialConditionImmune { pokemon = { id = "sv4_123", name = "Brute Bonnet" } })
            ]
        , describe "Timeout"
            [ test "parses timeout" <|
                \_ ->
                    parseAction "Jeodude1025 didn't take an action in time."
                        |> Expect.equal (Timeout { player = "Jeodude1025" })
            ]
        , describe "ShuffledDeck"
            [ test "parses shuffled deck" <|
                \_ ->
                    parseAction "NoxFoxEX shuffled their deck."
                        |> Expect.equal (ShuffledDeck { player = "NoxFoxEX" })
            ]
        , describe "ShuffledCards"
            [ test "parses shuffled cards" <|
                \_ ->
                    parseAction "zosiu shuffled their cards."
                        |> Expect.equal (ShuffledCards { player = "zosiu" })
            ]
        , describe "ShuffledInto"
            [ test "parses shuffled named card into deck" <|
                \_ ->
                    parseAction "zosiu shuffled (sv9_175) N's Zoroark ex into their deck."
                        |> Expect.equal
                            (ShuffledInto
                                { player = "zosiu"
                                , card = Just { id = "sv9_175", name = "N's Zoroark ex" }
                                , count = Nothing
                                }
                            )
            , test "parses shuffled N cards into deck" <|
                \_ ->
                    parseAction "Alannvs86 shuffled 5 cards into their deck."
                        |> Expect.equal
                            (ShuffledInto { player = "Alannvs86", card = Nothing, count = Just 5 })
            , test "parses shuffled a card into deck" <|
                \_ ->
                    parseAction "Alannvs86 shuffled a card into their deck."
                        |> Expect.equal
                            (ShuffledInto { player = "Alannvs86", card = Nothing, count = Just 1 })
            ]
        , describe "PutOnTop"
            [ test "parses put card on top of deck" <|
                \_ ->
                    parseAction "zosiu put (me2_56) Mega Gengar ex on top of their deck."
                        |> Expect.equal
                            (PutOnTop
                                { player = "zosiu"
                                , card = { id = "me2_56", name = "Mega Gengar ex" }
                                }
                            )
            ]
        , describe "PutOnBottom"
            [ test "parses put named card on bottom of deck" <|
                \_ ->
                    parseAction "zosiu put (me1_114_ph) Boss's Orders on the bottom of their deck."
                        |> Expect.equal
                            (PutOnBottom
                                { player = "zosiu"
                                , card = Just { id = "me1_114_ph", name = "Boss's Orders" }
                                , count = Nothing
                                }
                            )
            , test "parses put N cards on bottom of deck" <|
                \_ ->
                    parseAction "Grimjawxyz put 5 cards on the bottom of their deck."
                        |> Expect.equal
                            (PutOnBottom { player = "Grimjawxyz", card = Nothing, count = Just 5 })
            ]
        , describe "Discarded"
            [ test "parses discarded N cards" <|
                \_ ->
                    parseAction "NoxFoxEX discarded 3 cards."
                        |> Expect.equal (Discarded { player = "NoxFoxEX", count = 3 })
            ]
        , describe "DrewCount"
            [ test "parses drew N cards (detail)" <|
                \_ ->
                    parseAction "NoxFoxEX drew 6 cards."
                        |> Expect.equal (DrewCount { player = "NoxFoxEX", count = 6 })
            ]
        , describe "MovedToHand"
            [ test "parses moved named card to hand" <|
                \_ ->
                    parseAction "zosiu moved zosiu's (sv6-5_36) Okidogi ex to their hand."
                        |> Expect.equal
                            (MovedToHand
                                { player = "zosiu"
                                , card = Just { id = "sv6-5_36", name = "Okidogi ex" }
                                , count = Nothing
                                }
                            )
            , test "parses moved N cards to hand" <|
                \_ ->
                    parseAction "Jeodude1025 moved Jeodude1025's 3 cards to their hand."
                        |> Expect.equal
                            (MovedToHand { player = "Jeodude1025", card = Nothing, count = Just 3 })
            ]
        , describe "MovedToDiscard"
            [ test "parses moved cards to discard" <|
                \_ ->
                    parseAction "Jeodude1025 moved zosiu's 4 cards to the discard pile."
                        |> Expect.equal
                            (MovedToDiscard { mover = "Jeodude1025", owner = "zosiu", count = 4 })
            ]
        , describe "PlacedDamageCounters"
            [ test "parses placed N damage counters" <|
                \_ ->
                    parseAction "HardTreecko put 5 damage counters on HardTreecko's (sv8-5_72_sph) Drakloak."
                        |> Expect.equal
                            (PlacedDamageCounters
                                { player = "HardTreecko"
                                , pokemon = { player = "HardTreecko", card = { id = "sv8-5_72_sph", name = "Drakloak" } }
                                , count = 5
                                }
                            )
            , test "parses placed a damage counter" <|
                \_ ->
                    parseAction "HardTreecko put a damage counter on HardTreecko's (sv8-5_72_sph) Drakloak."
                        |> Expect.equal
                            (PlacedDamageCounters
                                { player = "HardTreecko"
                                , pokemon = { player = "HardTreecko", card = { id = "sv8-5_72_sph", name = "Drakloak" } }
                                , count = 1
                                }
                            )
            ]
        , describe "MovedDamageCounters"
            [ test "parses moved damage counters between pokemon" <|
                \_ ->
                    parseAction "zosiu moved 3 damage counters from zosiu's (sv8-5_168) Bloodmoon Ursaluna ex to zosiu's (sv7_118) Fan Rotom."
                        |> Expect.equal
                            (MovedDamageCounters
                                { player = "zosiu"
                                , count = 3
                                , from = { player = "zosiu", card = { id = "sv8-5_168", name = "Bloodmoon Ursaluna ex" } }
                                , to = { player = "zosiu", card = { id = "sv7_118", name = "Fan Rotom" } }
                                }
                            )
            ]
        , describe "CoinFlipResult"
            [ test "parses single coin flip heads" <|
                \_ ->
                    parseAction "ManuelZ1702 flipped a coin and it landed on heads."
                        |> Expect.equal (CoinFlipResult { player = "ManuelZ1702", flipped = 1, heads = 1 })
            , test "parses single coin flip tails" <|
                \_ ->
                    parseAction "animated_pasta flipped a coin and it landed on tails."
                        |> Expect.equal (CoinFlipResult { player = "animated_pasta", flipped = 1, heads = 0 })
            , test "parses multiple coin flips" <|
                \_ ->
                    parseAction "animated_pasta flipped 3 coins, and 2 landed on heads."
                        |> Expect.equal (CoinFlipResult { player = "animated_pasta", flipped = 3, heads = 2 })
            ]
        , describe "ChoseOption"
            [ test "parses chose option" <|
                \_ ->
                    parseAction "Grimjawxyz chose Flamebody Cannon"
                        |> Expect.equal (ChoseOption { player = "Grimjawxyz", option = "Flamebody Cannon" })
            ]
        , describe "UnknownAction"
            [ test "returns UnknownAction for unrecognised line" <|
                \_ ->
                    parseAction "Something totally unrecognised happened here."
                        |> Expect.equal (UnknownAction "Something totally unrecognised happened here.")
            ]
        , describe "groupLines"
            [ test "groups a top line with its detail and bullet sub-lines" <|
                \_ ->
                    let
                        lines =
                            [ TopLine "NoxFoxEX played (sv1_181_ph) Nest Ball."
                            , DetailLine "NoxFoxEX drew (sv4_123) Brute Bonnet and played it to the Bench."
                            , DetailLine "NoxFoxEX shuffled their deck."
                            ]

                        groups =
                            groupLines lines
                    in
                    groups
                        |> List.length
                        |> Expect.equal 1
            , test "a single group has the correct number of details" <|
                \_ ->
                    let
                        lines =
                            [ TopLine "NoxFoxEX played (sv1_181_ph) Nest Ball."
                            , DetailLine "NoxFoxEX drew (sv4_123) Brute Bonnet and played it to the Bench."
                            , DetailLine "NoxFoxEX shuffled their deck."
                            ]

                        groups =
                            groupLines lines
                    in
                    groups
                        |> List.head
                        |> Maybe.map (.details >> List.length)
                        |> Expect.equal (Just 2)
            , test "bullet lines are attached to the preceding detail" <|
                \_ ->
                    let
                        lines =
                            [ TopLine "NoxFoxEX played (sv1_181_ph) Nest Ball."
                            , DetailLine "NoxFoxEX discarded 3 cards."
                            , BulletLine "(sv2_254) Iono, (sv7_175) Bravery Charm"
                            ]

                        groups =
                            groupLines lines
                    in
                    groups
                        |> List.head
                        |> Maybe.andThen (.details >> List.head)
                        |> Maybe.map (.bullets >> List.length)
                        |> Expect.equal (Just 1)
            , test "multiple top lines produce multiple groups" <|
                \_ ->
                    let
                        lines =
                            [ TopLine "zosiu drew a card."
                            , TopLine "NoxFoxEX drew a card."
                            ]
                    in
                    groupLines lines
                        |> List.length
                        |> Expect.equal 2
            ]
        ]
