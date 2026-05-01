module Action exposing
    ( Action(..)
    , ActionGroup
    , BulletAction
    , CardRef
    , DetailAction
    , DamageModifier(..)
    , PokemonRef
    , Position(..)
    , groupLines
    , parseAction
    , parseCardRef
    )

import Replay exposing (ReplayLine(..))


-- SUPPORTING TYPES


type alias CardRef =
    { id : String
    , name : String
    }


type alias PokemonRef =
    { player : String
    , card : CardRef
    }


type Position
    = ActiveSpot
    | BenchSpot
    | StadiumSpot


type DamageModifier
    = Weakness String Int
    | Resistance String Int


-- GROUPED STRUCTURE


type alias BulletAction =
    { raw : String
    , action : Action
    }


type alias DetailAction =
    { raw : String
    , action : Action
    , bullets : List BulletAction
    }


type alias ActionGroup =
    { raw : String
    , action : Action
    , details : List DetailAction
    }


-- ACTION TYPE


type Action
    -- Setup
    = CoinFlipChoice { player : String, choice : String }
    | CoinTossWon { player : String }
    | GoDecision { player : String, first : Bool }
    | OpeningDraw { player : String, count : Int }
    | MulliganTaken { player : String, count : Int }
    | MulliganBonus { player : String, count : Int, because : String }
    -- Playing cards
    | PlayedPokemon { player : String, card : CardRef, position : Position }
    | PlayedStadium { player : String, card : CardRef }
    | UsedStadium { player : String, name : String }
    | PlayedTrainer { player : String, card : CardRef }
    -- Drawing / searching
    | Drew { player : String, card : Maybe CardRef }
    | DrewCount { player : String, count : Int }
    | DrewCard { player : String, card : CardRef, andPlayed : Maybe Position }
    | DrewAndPlayed { player : String, count : Int, position : Position }
    -- Attachment
    | Attached { player : String, item : CardRef, target : PokemonRef, position : Position }
    -- Evolution
    | Evolved { player : String, from : CardRef, to : CardRef, position : Position }
    -- Attacking
    | UsedAttack
        { attacker : PokemonRef
        , move : String
        , target : Maybe { defender : PokemonRef, damage : Int }
        , modifier : Maybe DamageModifier
        }
    -- Pokémon state
    | KnockedOut { pokemon : PokemonRef }
    | TookDamage { pokemon : PokemonRef, amount : Int }
    | ConditionApplied { pokemon : PokemonRef, condition : String }
    | ConditionRemoved { pokemon : PokemonRef, condition : String }
    -- Position changes
    | MovedToActive { pokemon : PokemonRef }
    | Retreated { player : String, card : CardRef }
    | Switched { player : String, from : CardRef, to : CardRef }
    -- Card movement
    | CardActivated { card : CardRef }
    | CardDiscardedFrom { card : CardRef, pokemon : PokemonRef }
    | CardAddedToHand { card : Maybe CardRef, player : String }
    | MovedToHand { player : String, card : Maybe CardRef, count : Maybe Int }
    | MovedToDiscard { mover : String, owner : String, count : Int }
    | PutOnTop { player : String, card : CardRef }
    | PutOnBottom { player : String, card : Maybe CardRef, count : Maybe Int }
    | ShuffledInto { player : String, card : Maybe CardRef, count : Maybe Int }
    | ShuffledDeck { player : String }
    | ShuffledCards { player : String }
    | Discarded { player : String, count : Int }
    | DiscardedCard { player : String, card : CardRef }
    -- Prizes
    | TookPrize { player : String, count : Int }
    -- Damage counters
    | PoisonCheckupDamage { pokemon : PokemonRef, counters : Int }
    | PlacedDamageCounters { player : String, pokemon : PokemonRef, count : Int }
    | MovedDamageCounters { player : String, count : Int, from : PokemonRef, to : PokemonRef }
    -- Effect negation
    | DamagePrevented { pokemon : CardRef }
    | EffectBlocked { move : String, pokemon : CardRef }
    | SpecialConditionImmune { pokemon : CardRef }
    -- Turn / misc
    | TurnEnded { player : String }
    | Timeout { player : String }
    | ChoseOption { player : String, option : String }
    | CoinFlipResult { player : String, flipped : Int, heads : Int }
    | CardList (List CardRef)
    | UnknownAction String


-- GROUPING


groupLines : List ReplayLine -> List ActionGroup
groupLines lines =
    groupHelp lines []


groupHelp : List ReplayLine -> List ActionGroup -> List ActionGroup
groupHelp lines acc =
    case lines of
        [] ->
            List.reverse acc

        (TopLine raw) :: rest ->
            let
                action =
                    parseAction raw

                ( details, remaining ) =
                    collectDetails rest []

                -- Prize-taking lines ("X took N Prize cards.") are always followed
                -- by one TopLine per card ("(id) Card was added to X's hand." /
                -- "A card was added to X's hand.").  Absorb those into this group
                -- as details so they appear as a single entry in the log.
                ( prizeDetails, finalRemaining ) =
                    case action of
                        TookPrize _ ->
                            collectCardAddedToHand remaining []

                        _ ->
                            ( [], remaining )
            in
            groupHelp finalRemaining
                ({ raw = raw
                 , action = action
                 , details = details ++ prizeDetails
                 }
                    :: acc
                )

        _ :: rest ->
            -- DetailLine or BulletLine without a preceding TopLine — skip
            groupHelp rest acc


{-| Greedily consume consecutive TopLines that parse as CardAddedToHand and
convert them into DetailAction entries (with no bullets).
-}
collectCardAddedToHand : List ReplayLine -> List DetailAction -> ( List DetailAction, List ReplayLine )
collectCardAddedToHand lines acc =
    case lines of
        (TopLine raw) :: rest ->
            case parseAction raw of
                CardAddedToHand _ ->
                    collectCardAddedToHand rest ({ raw = raw, action = parseAction raw, bullets = [] } :: acc)

                _ ->
                    ( List.reverse acc, lines )

        _ ->
            ( List.reverse acc, lines )


collectDetails : List ReplayLine -> List DetailAction -> ( List DetailAction, List ReplayLine )
collectDetails lines acc =
    case lines of
        [] ->
            ( List.reverse acc, [] )

        (TopLine _) :: _ ->
            ( List.reverse acc, lines )

        (DetailLine raw) :: rest ->
            let
                ( bullets, remaining ) =
                    collectBullets rest []

                detail =
                    { raw = raw, action = parseAction raw, bullets = bullets }
            in
            collectDetails remaining (detail :: acc)

        (BulletLine raw) :: rest ->
            -- Bullet without a preceding DetailLine — attach as a detail with no bullets
            let
                detail =
                    { raw = raw, action = parseAction raw, bullets = [] }
            in
            collectDetails rest (detail :: acc)


collectBullets : List ReplayLine -> List BulletAction -> ( List BulletAction, List ReplayLine )
collectBullets lines acc =
    case lines of
        (BulletLine raw) :: rest ->
            collectBullets rest ({ raw = raw, action = parseAction raw } :: acc)

        _ ->
            ( List.reverse acc, lines )


-- PARSING


parseAction : String -> Action
parseAction raw =
    -- Try each matcher in priority order
    tryKnockedOut raw
        |> orTry (tryTurnEnded raw)
        |> orTry (tryUsedAttack raw)
        |> orTry (tryPlayedPokemon raw)
        |> orTry (tryPlayedStadium raw)
        |> orTry (tryPlayedTrainer raw)
        |> orTry (tryUsedStadium raw)
        |> orTry (tryEvolved raw)
        |> orTry (tryAttached raw)
        |> orTry (tryRetreated raw)
        |> orTry (trySwitched raw)
        |> orTry (tryMovedToActive raw)
        |> orTry (tryConditionApplied raw)
        |> orTry (tryConditionRemoved raw)
        |> orTry (tryTookDamage raw)
        |> orTry (tryCardActivated raw)
        |> orTry (tryCardDiscardedFrom raw)
        |> orTry (tryCardAddedToHandNamed raw)
        |> orTry (tryCardAddedToHandHidden raw)
        |> orTry (trySpecialConditionImmune raw)
        |> orTry (tryOpeningDraw raw)
        |> orTry (tryCoinFlipChoice raw)
        |> orTry (tryCoinTossWon raw)
        |> orTry (tryGoDecision raw)
        |> orTry (tryMulliganBonus raw)
        |> orTry (tryMulliganTaken raw)
        |> orTry (tryTookPrize raw)
        |> orTry (tryPoisonCheckupDamage raw)
        |> orTry (tryDamagePrevented raw)
        |> orTry (tryEffectBlocked raw)
        |> orTry (tryDrew raw)
        |> orTry (tryDrewCard raw)
        |> orTry (tryDrewAndPlayed raw)
        |> orTry (tryDrewCount raw)
        |> orTry (tryDiscarded raw)
        |> orTry (tryDiscardedCard raw)
        |> orTry (tryShuffledDeck raw)
        |> orTry (tryShuffledCards raw)
        |> orTry (tryShuffledInto raw)
        |> orTry (tryPutOnTop raw)
        |> orTry (tryPutOnBottom raw)
        |> orTry (tryPlacedDamageCounters raw)
        |> orTry (tryMovedDamageCounters raw)
        |> orTry (tryMovedToHand raw)
        |> orTry (tryMovedToDiscard raw)
        |> orTry (tryCoinFlipResult raw)
        |> orTry (tryChoseOption raw)
        |> orTry (tryTimeout raw)
        |> orTry (tryCardList raw)
        |> Maybe.withDefault (UnknownAction raw)


orTry : Maybe a -> Maybe a -> Maybe a
orTry second first =
    case first of
        Just _ ->
            first

        Nothing ->
            second


-- MATCHERS
-- Each returns Maybe Action, Nothing = no match


tryKnockedOut : String -> Maybe Action
tryKnockedOut raw =
    -- "PLAYER's (id) Name was Knocked Out!"
    if String.endsWith " was Knocked Out!" raw then
        case parsePokemonRef (String.dropRight 17 raw) of
            Just pokemon ->
                Just (KnockedOut { pokemon = pokemon })

            Nothing ->
                Nothing

    else
        Nothing


tryTurnEnded : String -> Maybe Action
tryTurnEnded raw =
    -- "PLAYER ended their turn."
    if String.endsWith " ended their turn." raw then
        let
            player =
                String.dropRight 18 raw
        in
        if String.isEmpty player then
            Nothing

        else
            Just (TurnEnded { player = player })

    else
        Nothing


tryUsedAttack : String -> Maybe Action
tryUsedAttack raw =
    -- "PLAYER's (id) Name used Move [on PLAYER's (id) Name for N damage[. modifier]]"
    case String.split " used " raw of
        [ attackerPart, rest ] ->
            case parsePokemonRef attackerPart of
                Just attacker ->
                    case String.split " on " rest of
                        [ move, targetAndDamage ] ->
                            case String.split " for " targetAndDamage of
                                [ targetPart, damageAndMore ] ->
                                    case parsePokemonRef targetPart of
                                        Just defender ->
                                            let
                                                ( damage, modifier ) =
                                                    parseDamageAndModifier damageAndMore
                                            in
                                            Just
                                                (UsedAttack
                                                    { attacker = attacker
                                                    , move = move
                                                    , target = Just { defender = defender, damage = damage }
                                                    , modifier = modifier
                                                    }
                                                )

                                        Nothing ->
                                            -- "on" was part of the move name, not a separator
                                            Just
                                                (UsedAttack
                                                    { attacker = attacker
                                                    , move = rest
                                                    , target = Nothing
                                                    , modifier = Nothing
                                                    }
                                                )

                                _ ->
                                    Nothing

                        _ ->
                            -- No target
                            Just
                                (UsedAttack
                                    { attacker = attacker
                                    , move = String.trimRight rest
                                    , target = Nothing
                                    , modifier = Nothing
                                    }
                                )

                Nothing ->
                    Nothing

        _ ->
            Nothing


parseDamageAndModifier : String -> ( Int, Maybe DamageModifier )
parseDamageAndModifier str =
    -- e.g. "260 damage." or "360 damage. Defender took 180 more damage because of Darkness Weakness."
    --      "0 damage. Defender took -30 less damage because of Fighting Resistance."
    let
        parts =
            String.split "." str

        damageStr =
            parts
                |> List.head
                |> Maybe.withDefault ""
                |> String.replace " damage" ""
                |> String.trim

        damage =
            String.toInt damageStr |> Maybe.withDefault 0

        modifier =
            parts
                |> List.tail
                |> Maybe.andThen List.head
                |> Maybe.andThen parseModifierSentence
    in
    ( damage, modifier )


parseModifierSentence : String -> Maybe DamageModifier
parseModifierSentence sentence =
    -- " Defender took 180 more damage because of Darkness Weakness."
    -- " Defender took -30 less damage because of Fighting Resistance."
    if String.contains " because of " sentence then
        let
            afterBecause =
                sentence
                    |> String.split " because of "
                    |> List.reverse
                    |> List.head
                    |> Maybe.withDefault ""
                    |> String.trimRight
                    |> String.replace "." ""
                    |> String.trim

            amount =
                if String.contains " more damage" sentence then
                    sentence
                        |> String.split " more damage"
                        |> List.head
                        |> Maybe.withDefault ""
                        |> String.words
                        |> List.reverse
                        |> List.head
                        |> Maybe.andThen String.toInt

                else if String.contains " less damage" sentence then
                    sentence
                        |> String.split " less damage"
                        |> List.head
                        |> Maybe.withDefault ""
                        |> String.words
                        |> List.reverse
                        |> List.head
                        |> Maybe.andThen (\s -> String.toInt (String.replace "-" "" s))

                else
                    Nothing
        in
        case amount of
            Just n ->
                if String.contains "Weakness" afterBecause then
                    let
                        typeName =
                            String.replace " Weakness" "" afterBecause
                    in
                    Just (Weakness typeName n)

                else if String.contains "Resistance" afterBecause then
                    let
                        typeName =
                            String.replace " Resistance" "" afterBecause
                    in
                    Just (Resistance typeName n)

                else
                    Nothing

            Nothing ->
                Nothing

    else
        Nothing


tryPlayedPokemon : String -> Maybe Action
tryPlayedPokemon raw =
    -- "PLAYER played (id) Name to the Active Spot." / "…to the Bench."
    if String.contains " played (" raw then
        if String.contains " to the Active Spot." raw then
            case String.split " played " raw of
                [ player, rest ] ->
                    parseCardRef rest
                        |> Maybe.map (\card -> PlayedPokemon { player = player, card = card, position = ActiveSpot })

                _ ->
                    Nothing

        else if String.contains " to the Bench." raw then
            case String.split " played " raw of
                [ player, rest ] ->
                    parseCardRef rest
                        |> Maybe.map (\card -> PlayedPokemon { player = player, card = card, position = BenchSpot })

                _ ->
                    Nothing

        else
            Nothing

    else
        Nothing


tryPlayedStadium : String -> Maybe Action
tryPlayedStadium raw =
    -- "PLAYER played (id) Name to the Stadium spot."
    if String.contains " played (" raw && String.contains " to the Stadium spot." raw then
        case String.split " played " raw of
            [ player, rest ] ->
                parseCardRef rest
                    |> Maybe.map (\card -> PlayedStadium { player = player, card = card })

            _ ->
                Nothing

    else
        Nothing


tryPlayedTrainer : String -> Maybe Action
tryPlayedTrainer raw =
    -- "PLAYER played (id) Name."
    if String.contains " played (" raw then
        case String.split " played " raw of
            [ player, rest ] ->
                parseCardRef rest
                    |> Maybe.map (\card -> PlayedTrainer { player = player, card = card })

            _ ->
                Nothing

    else
        Nothing


tryUsedStadium : String -> Maybe Action
tryUsedStadium raw =
    -- "PLAYER played Name."  (no card ID — stadium re-use / ability)
    if String.contains " played " raw && not (String.contains " played (" raw) && not (String.contains " played it " raw) && not (String.contains " and played " raw) then
        case String.split " played " raw of
            [ player, nameDot ] ->
                let
                    name =
                        String.trimRight (String.replace "." "" nameDot)
                in
                if String.isEmpty name then
                    Nothing

                else
                    Just (UsedStadium { player = player, name = name })

            _ ->
                Nothing

    else
        Nothing


tryEvolved : String -> Maybe Action
tryEvolved raw =
    -- "PLAYER evolved (from-id) From to (to-id) To on the Bench."
    -- "PLAYER evolved (from-id) From to (to-id) To in the Active Spot."
    if String.contains " evolved (" raw then
        let
            position =
                if String.contains " on the Bench." raw then
                    Just BenchSpot

                else if String.contains " in the Active Spot." raw then
                    Just ActiveSpot

                else
                    Nothing
        in
        case position of
            Nothing ->
                Nothing

            Just pos ->
                case String.split " evolved " raw of
                    [ player, rest ] ->
                        -- rest = "(from-id) From to (to-id) To on the Bench."
                        case String.split " to (" rest of
                            [ fromPart, toPart ] ->
                                let
                                    fromCard =
                                        parseCardRef fromPart

                                    toCard =
                                        parseCardRef ("(" ++ toPart)
                                in
                                case ( fromCard, toCard ) of
                                    ( Just from, Just to ) ->
                                        Just (Evolved { player = player, from = from, to = to, position = pos })

                                    _ ->
                                        Nothing

                            _ ->
                                Nothing

                    _ ->
                        Nothing

    else
        Nothing


tryAttached : String -> Maybe Action
tryAttached raw =
    -- "PLAYER attached (item-id) Item to (pokemon-id) Pokemon on the Bench."
    -- "PLAYER attached (item-id) Item to (pokemon-id) Pokemon in the Active Spot."
    if String.contains " attached (" raw then
        let
            position =
                if String.contains " on the Bench." raw then
                    Just BenchSpot

                else if String.contains " in the Active Spot." raw then
                    Just ActiveSpot

                else
                    Nothing
        in
        case position of
            Nothing ->
                Nothing

            Just pos ->
                case String.split " attached " raw of
                    [ player, rest ] ->
                        -- rest = "(item-id) Item to (pokemon-id) Pokemon on the Bench."
                        case String.split " to (" rest of
                            [ itemPart, pokemonPart ] ->
                                let
                                    item =
                                        parseCardRef itemPart

                                    pokemonCard =
                                        parseCardRef ("(" ++ pokemonPart)
                                in
                                case ( item, pokemonCard ) of
                                    ( Just itemCard, Just pCard ) ->
                                        let
                                            pokemon =
                                                { player = player, card = pCard }
                                        in
                                        Just (Attached { player = player, item = itemCard, target = pokemon, position = pos })

                                    _ ->
                                        Nothing

                            _ ->
                                Nothing

                    _ ->
                        Nothing

    else
        Nothing


tryRetreated : String -> Maybe Action
tryRetreated raw =
    -- "PLAYER retreated (id) Name to the Bench."
    if String.contains " retreated (" raw && String.endsWith " to the Bench." raw then
        case String.split " retreated " raw of
            [ player, rest ] ->
                parseCardRef rest
                    |> Maybe.map (\card -> Retreated { player = player, card = card })

            _ ->
                Nothing

    else
        Nothing


trySwitched : String -> Maybe Action
trySwitched raw =
    -- "PLAYER's (from-id) From was switched with PLAYER's (to-id) To to become the Active Pokémon."
    if String.contains " was switched with " raw && String.endsWith " to become the Active Pokémon." raw then
        let
            withoutSuffix =
                String.dropRight 30 raw

            -- withoutSuffix = "PLAYER's (from-id) From was switched with PLAYER's (to-id) To"
        in
        case String.split " was switched with " withoutSuffix of
            [ fromFull, toFull ] ->
                case ( parsePokemonRef fromFull, parsePokemonRef toFull ) of
                    ( Just fromRef, Just _ ) ->
                        Just (Switched { player = fromRef.player, from = fromRef.card, to = { id = "", name = "" } })

                    _ ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


tryMovedToActive : String -> Maybe Action
tryMovedToActive raw =
    -- "PLAYER's (id) Name is now in the Active Spot."
    if String.endsWith " is now in the Active Spot." raw then
        parsePokemonRef (String.dropRight 27 raw)
            |> Maybe.map (\pokemon -> MovedToActive { pokemon = pokemon })

    else
        Nothing


tryConditionApplied : String -> Maybe Action
tryConditionApplied raw =
    -- "PLAYER's (id) Name is now Poisoned/Confused/Burned/Asleep/Paralyzed."
    if String.contains " is now " raw && not (String.contains " is now in the Active Spot." raw) then
        case String.split " is now " raw of
            [ pokemonPart, conditionDot ] ->
                case parsePokemonRef pokemonPart of
                    Just pokemon ->
                        let
                            condition =
                                String.trimRight conditionDot |> String.replace "." ""
                        in
                        Just (ConditionApplied { pokemon = pokemon, condition = condition })

                    Nothing ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


tryConditionRemoved : String -> Maybe Action
tryConditionRemoved raw =
    -- "PLAYER's (id) Name is no longer Poisoned."
    if String.contains " is no longer " raw then
        case String.split " is no longer " raw of
            [ pokemonPart, conditionDot ] ->
                case parsePokemonRef pokemonPart of
                    Just pokemon ->
                        let
                            condition =
                                String.trimRight conditionDot |> String.replace "." ""
                        in
                        Just (ConditionRemoved { pokemon = pokemon, condition = condition })

                    Nothing ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


tryTookDamage : String -> Maybe Action
tryTookDamage raw =
    -- "PLAYER's (id) Name took N damage."
    if String.endsWith " damage." raw && String.contains " took " raw && String.contains "'s (" raw then
        case String.split " took " raw of
            [ pokemonPart, amountDamage ] ->
                case parsePokemonRef pokemonPart of
                    Just pokemon ->
                        let
                            amountStr =
                                amountDamage
                                    |> String.replace " damage." ""
                                    |> String.replace " more" ""
                                    |> String.replace " less" ""
                                    |> String.trim
                        in
                        case String.toInt amountStr of
                            Just amount ->
                                Just (TookDamage { pokemon = pokemon, amount = amount })

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


tryCardActivated : String -> Maybe Action
tryCardActivated raw =
    -- "(id) Name was activated."
    if String.startsWith "(" raw && String.endsWith " was activated." raw then
        parseCardRef raw
            |> Maybe.map (\card -> CardActivated { card = card })

    else
        Nothing


tryCardDiscardedFrom : String -> Maybe Action
tryCardDiscardedFrom raw =
    -- "(card-id) Card was discarded from PLAYER's (pokemon-id) Pokemon."
    if String.startsWith "(" raw && String.contains " was discarded from " raw then
        case String.split " was discarded from " raw of
            [ cardPart, pokemonFull ] ->
                case ( parseCardRef cardPart, parsePokemonRef pokemonFull ) of
                    ( Just card, Just pokemon ) ->
                        Just (CardDiscardedFrom { card = card, pokemon = pokemon })

                    _ ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


tryCardAddedToHandNamed : String -> Maybe Action
tryCardAddedToHandNamed raw =
    -- "(id) Name was added to PLAYER's hand."
    if String.startsWith "(" raw && String.contains " was added to " raw && String.endsWith "'s hand." raw then
        case String.split " was added to " raw of
            [ cardPart, playerHand ] ->
                case parseCardRef cardPart of
                    Just card ->
                        let
                            player =
                                String.dropRight 8 playerHand
                        in
                        Just (CardAddedToHand { card = Just card, player = player })

                    Nothing ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


tryCardAddedToHandHidden : String -> Maybe Action
tryCardAddedToHandHidden raw =
    -- "A card was added to PLAYER's hand."
    if String.startsWith "A card was added to " raw && String.endsWith "'s hand." raw then
        let
            player =
                raw
                    |> String.dropLeft 20
                    |> String.dropRight 8
        in
        if String.isEmpty player then
            Nothing

        else
            Just (CardAddedToHand { card = Nothing, player = player })

    else
        Nothing


trySpecialConditionImmune : String -> Maybe Action
trySpecialConditionImmune raw =
    -- "(id) Pokemon cannot be affected by Special Conditions."
    if String.startsWith "(" raw && String.endsWith " cannot be affected by Special Conditions." raw then
        parseCardRef raw
            |> Maybe.map (\card -> SpecialConditionImmune { pokemon = card })

    else
        Nothing


tryOpeningDraw : String -> Maybe Action
tryOpeningDraw raw =
    -- "PLAYER drew N cards for the opening hand."
    if String.endsWith " for the opening hand." raw then
        -- "PLAYER drew N cards" → split on " drew "
        case String.split " drew " raw of
            [ player, countAndRest ] ->
                let
                    countStr =
                        countAndRest
                            |> String.split " cards"
                            |> List.head
                            |> Maybe.withDefault ""

                    count =
                        case countStr of
                            "a" ->
                                Just 1

                            _ ->
                                String.toInt countStr
                in
                case count of
                    Just n ->
                        Just (OpeningDraw { player = player, count = n })

                    Nothing ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


tryCoinFlipChoice : String -> Maybe Action
tryCoinFlipChoice raw =
    -- "PLAYER chose heads/tails for the opening coin flip."
    if String.endsWith " for the opening coin flip." raw then
        case String.split " chose " raw of
            [ player, choicePart ] ->
                let
                    choice =
                        choicePart
                            |> String.split " for"
                            |> List.head
                            |> Maybe.withDefault ""
                in
                Just (CoinFlipChoice { player = player, choice = choice })

            _ ->
                Nothing

    else
        Nothing


tryCoinTossWon : String -> Maybe Action
tryCoinTossWon raw =
    -- "PLAYER won the coin toss."
    if String.endsWith " won the coin toss." raw then
        let
            player =
                String.dropRight 19 raw
        in
        if String.isEmpty player then
            Nothing

        else
            Just (CoinTossWon { player = player })

    else
        Nothing


tryGoDecision : String -> Maybe Action
tryGoDecision raw =
    -- "PLAYER decided to go first." / "…second."
    if String.contains " decided to go " raw then
        case String.split " decided to go " raw of
            [ player, choice ] ->
                let
                    first =
                        String.startsWith "first" choice
                in
                Just (GoDecision { player = player, first = first })

            _ ->
                Nothing

    else
        Nothing


tryMulliganBonus : String -> Maybe Action
tryMulliganBonus raw =
    -- "PLAYER drew N more cards because OPPONENT took at least 1 mulligan."
    if String.contains " more cards because " raw then
        case String.split " drew " raw of
            [ player, rest ] ->
                case String.split " more cards because " rest of
                    [ countStr, because ] ->
                        case String.toInt (String.trim countStr) of
                            Just count ->
                                Just
                                    (MulliganBonus
                                        { player = player
                                        , count = count
                                        , because = String.trimRight because |> String.replace "." ""
                                        }
                                    )

                            Nothing ->
                                Nothing

                    _ ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


tryMulliganTaken : String -> Maybe Action
tryMulliganTaken raw =
    -- "PLAYER took a mulligan." / "PLAYER took N mulligans."
    if String.contains " took a mulligan" raw || String.contains " took " raw && String.contains " mulligan" raw then
        case String.split " took " raw of
            [ player, rest ] ->
                let
                    count =
                        if String.startsWith "a mulligan" rest then
                            Just 1

                        else
                            rest
                                |> String.split " mulligan"
                                |> List.head
                                |> Maybe.andThen String.toInt
                in
                case count of
                    Just n ->
                        Just (MulliganTaken { player = player, count = n })

                    Nothing ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


tryTookPrize : String -> Maybe Action
tryTookPrize raw =
    -- "PLAYER took a Prize card." / "PLAYER took N Prize cards."
    if String.contains " Prize card" raw && String.contains " took " raw then
        case String.split " took " raw of
            [ player, rest ] ->
                let
                    count =
                        if String.startsWith "a Prize" rest then
                            Just 1

                        else
                            rest
                                |> String.split " Prize"
                                |> List.head
                                |> Maybe.andThen String.toInt
                in
                case count of
                    Just n ->
                        Just (TookPrize { player = player, count = n })

                    Nothing ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


tryPoisonCheckupDamage : String -> Maybe Action
tryPoisonCheckupDamage raw =
    -- "N damage counter(s) was/were placed on PLAYER's (id) Pokemon for the Special Condition Poisoned."
    if String.contains "damage counter" raw && String.contains "Special Condition Poisoned" raw then
        case String.split " placed on " raw of
            [ countPart, pokemonAndRest ] ->
                let
                    counters =
                        if String.startsWith "1 damage counter" countPart then
                            Just 1

                        else
                            countPart
                                |> String.split " damage"
                                |> List.head
                                |> Maybe.andThen String.toInt
                in
                case counters of
                    Just n ->
                        case String.split " for the Special Condition" pokemonAndRest of
                            [ pokemonFull, _ ] ->
                                parsePokemonRef pokemonFull
                                    |> Maybe.map (\pokemon -> PoisonCheckupDamage { pokemon = pokemon, counters = n })

                            _ ->
                                Nothing

                    Nothing ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


tryDamagePrevented : String -> Maybe Action
tryDamagePrevented raw =
    -- "Damage to (id) Pokemon was prevented."
    if String.startsWith "Damage to (" raw && String.endsWith " was prevented." raw then
        let
            inner =
                raw
                    |> String.dropLeft 10
                    |> String.dropRight 15
        in
        parseCardRef inner
            |> Maybe.map (\card -> DamagePrevented { pokemon = card })

    else
        Nothing


tryEffectBlocked : String -> Maybe Action
tryEffectBlocked raw =
    -- "Effects of MOVE did not affect (id) Pokemon."
    if String.startsWith "Effects of " raw && String.contains " did not affect (" raw then
        case String.split " did not affect " raw of
            [ movePart, pokemonPart ] ->
                let
                    move =
                        String.dropLeft 11 movePart
                in
                parseCardRef pokemonPart
                    |> Maybe.map (\card -> EffectBlocked { move = move, pokemon = card })

            _ ->
                Nothing

    else
        Nothing


tryDrew : String -> Maybe Action
tryDrew raw =
    -- "PLAYER drew a card."  or  "PLAYER drew (id) Name."
    -- Lines with "and played it" are handled by tryDrewCard instead
    if String.contains " drew a card." raw then
        case String.split " drew a card." raw of
            [ player, _ ] ->
                Just (Drew { player = player, card = Nothing })

            _ ->
                Nothing

    else if String.contains " drew (" raw && not (String.contains " and played it to " raw) then
        case String.split " drew " raw of
            [ player, rest ] ->
                parseCardRef rest
                    |> Maybe.map (\card -> Drew { player = player, card = Just card })

            _ ->
                Nothing

    else
        Nothing


tryDrewCard : String -> Maybe Action
tryDrewCard raw =
    -- Detail: "PLAYER drew (id) Name and played it to POSITION."
    -- (lines without "and played it" are handled by tryDrew as Drew)
    if String.contains " drew (" raw && String.contains " and played it to " raw then
        case String.split " drew " raw of
            [ player, rest ] ->
                let
                    andPlayed =
                        if String.contains " and played it to the Active Spot." rest then
                            Just ActiveSpot

                        else if String.contains " and played it to the Bench." rest then
                            Just BenchSpot

                        else
                            Nothing
                in
                case parseCardRef rest of
                    Just card ->
                        Just (DrewCard { player = player, card = card, andPlayed = andPlayed })

                    Nothing ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


tryDrewAndPlayed : String -> Maybe Action
tryDrewAndPlayed raw =
    -- "PLAYER drew N cards and played them to the Bench."
    -- "PLAYER drew N cards and played them to the Active Spot."
    -- (singular: "drew 1 card and played it to …")
    let
        toBench =
            String.endsWith " to the Bench." raw

        toActive =
            String.endsWith " to the Active Spot." raw
    in
    if (toBench || toActive) && String.contains " drew " raw && String.contains " and played " raw then
        case String.split " drew " raw of
            [ player, rest ] ->
                let
                    countStr =
                        rest
                            |> String.split " card"
                            |> List.head
                            |> Maybe.withDefault ""
                            |> String.trim
                in
                String.toInt countStr
                    |> Maybe.map
                        (\n ->
                            DrewAndPlayed
                                { player = player
                                , count = n
                                , position =
                                    if toBench then
                                        BenchSpot

                                    else
                                        ActiveSpot
                                }
                        )

            _ ->
                Nothing

    else
        Nothing


tryDrewCount : String -> Maybe Action
tryDrewCount raw =
    -- Detail: "PLAYER drew N cards."
    if String.contains " drew " raw && String.endsWith " cards." raw then
        case String.split " drew " raw of
            [ player, rest ] ->
                let
                    countStr =
                        String.replace " cards." "" rest |> String.trim
                in
                case String.toInt countStr of
                    Just n ->
                        Just (DrewCount { player = player, count = n })

                    Nothing ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


tryDiscarded : String -> Maybe Action
tryDiscarded raw =
    -- "PLAYER discarded N cards."
    if String.contains " discarded " raw && String.endsWith " cards." raw && not (String.contains "(" raw) then
        case String.split " discarded " raw of
            [ player, rest ] ->
                let
                    countStr =
                        String.replace " cards." "" rest |> String.trim
                in
                case String.toInt countStr of
                    Just n ->
                        Just (Discarded { player = player, count = n })

                    Nothing ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


tryDiscardedCard : String -> Maybe Action
tryDiscardedCard raw =
    -- "PLAYER discarded (id) Name."
    if String.contains " discarded (" raw then
        case String.split " discarded " raw of
            [ player, rest ] ->
                parseCardRef rest
                    |> Maybe.map (\card -> DiscardedCard { player = player, card = card })

            _ ->
                Nothing

    else
        Nothing


tryShuffledDeck : String -> Maybe Action
tryShuffledDeck raw =
    -- "PLAYER shuffled their deck."
    if String.endsWith " shuffled their deck." raw then
        let
            player =
                String.dropRight 21 raw
        in
        if String.isEmpty player then
            Nothing

        else
            Just (ShuffledDeck { player = player })

    else
        Nothing


tryShuffledCards : String -> Maybe Action
tryShuffledCards raw =
    -- "PLAYER shuffled their cards."
    if String.endsWith " shuffled their cards." raw then
        let
            player =
                String.dropRight 22 raw
        in
        if String.isEmpty player then
            Nothing

        else
            Just (ShuffledCards { player = player })

    else
        Nothing


tryShuffledInto : String -> Maybe Action
tryShuffledInto raw =
    -- "PLAYER shuffled (id) Card into their deck."
    -- "PLAYER shuffled N cards into their deck."
    -- "PLAYER shuffled a card into their deck."
    if String.contains " shuffled " raw && String.endsWith " into their deck." raw then
        case String.split " shuffled " raw of
            [ player, rest ] ->
                let
                    content =
                        String.replace " into their deck." "" rest |> String.trim
                in
                if String.startsWith "(" content then
                    parseCardRef content
                        |> Maybe.map (\card -> ShuffledInto { player = player, card = Just card, count = Nothing })

                else if content == "a card" then
                    Just (ShuffledInto { player = player, card = Nothing, count = Just 1 })

                else
                    case content |> String.split " " |> List.head |> Maybe.andThen String.toInt of
                        Just n ->
                            Just (ShuffledInto { player = player, card = Nothing, count = Just n })

                        Nothing ->
                            Nothing

            _ ->
                Nothing

    else
        Nothing


tryPutOnTop : String -> Maybe Action
tryPutOnTop raw =
    -- "PLAYER put (id) Name on top of their deck."
    if String.contains " put (" raw && String.contains " on top of their deck." raw then
        case String.split " put " raw of
            [ player, rest ] ->
                parseCardRef rest
                    |> Maybe.map (\card -> PutOnTop { player = player, card = card })

            _ ->
                Nothing

    else
        Nothing


tryPutOnBottom : String -> Maybe Action
tryPutOnBottom raw =
    -- "PLAYER put (id) Name on the bottom of their deck."
    -- "PLAYER put N cards on the bottom of their deck."
    if String.contains " put " raw && String.contains " on the bottom of their deck." raw then
        case String.split " put " raw of
            [ player, rest ] ->
                if String.startsWith "(" rest then
                    parseCardRef rest
                        |> Maybe.map (\card -> PutOnBottom { player = player, card = Just card, count = Nothing })

                else
                    let
                        countStr =
                            rest
                                |> String.split " card"
                                |> List.head
                                |> Maybe.withDefault ""
                                |> String.trim
                    in
                    case countStr of
                        "a" ->
                            Just (PutOnBottom { player = player, card = Nothing, count = Just 1 })

                        _ ->
                            case String.toInt countStr of
                                Just n ->
                                    Just (PutOnBottom { player = player, card = Nothing, count = Just n })

                                Nothing ->
                                    Nothing

            _ ->
                Nothing

    else
        Nothing


tryPlacedDamageCounters : String -> Maybe Action
tryPlacedDamageCounters raw =
    -- "PLAYER put N damage counters on PLAYER's (id) Pokemon."
    -- "PLAYER put a damage counter on PLAYER's (id) Pokemon."
    if String.contains " put " raw && String.contains "damage counter" raw then
        case String.split " put " raw of
            [ mover, rest ] ->
                let
                    count =
                        if String.startsWith "a damage counter" rest then
                            Just 1

                        else
                            rest
                                |> String.split " damage counter"
                                |> List.head
                                |> Maybe.andThen String.toInt
                in
                case count of
                    Just n ->
                        case String.split " on " rest of
                            [ _, pokemonFull ] ->
                                parsePokemonRef pokemonFull
                                    |> Maybe.map
                                        (\pokemon ->
                                            PlacedDamageCounters
                                                { player = mover
                                                , pokemon = pokemon
                                                , count = n
                                                }
                                        )

                            _ ->
                                Nothing

                    Nothing ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


tryMovedDamageCounters : String -> Maybe Action
tryMovedDamageCounters raw =
    -- "PLAYER moved N damage counters from PLAYER's (id) A to PLAYER's (id) B."
    if String.contains " moved " raw && String.contains "damage counters" raw && String.contains " from " raw then
        case String.split " moved " raw of
            [ mover, rest ] ->
                let
                    count =
                        rest
                            |> String.split " damage counter"
                            |> List.head
                            |> Maybe.andThen String.toInt
                in
                case count of
                    Just n ->
                        case String.split " from " rest of
                            [ _, fromToStr ] ->
                                case String.split " to " fromToStr of
                                    [ fromFull, toFull ] ->
                                        case ( parsePokemonRef fromFull, parsePokemonRef (String.trimRight toFull |> String.replace "." "") ) of
                                            ( Just fromRef, Just toRef ) ->
                                                Just
                                                    (MovedDamageCounters
                                                        { player = mover
                                                        , count = n
                                                        , from = fromRef
                                                        , to = toRef
                                                        }
                                                    )

                                            _ ->
                                                Nothing

                                    _ ->
                                        Nothing

                            _ ->
                                Nothing

                    Nothing ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


tryMovedToHand : String -> Maybe Action
tryMovedToHand raw =
    -- "PLAYER moved PLAYER's (card-id) Card to their hand."
    -- "PLAYER moved PLAYER's N cards to their hand."
    if String.contains " moved " raw && String.endsWith " to their hand." raw then
        case String.split " moved " raw of
            [ mover, rest ] ->
                -- rest = "PLAYER's (id) Name to their hand." or "PLAYER's N cards to their hand."
                case String.split "'s " rest of
                    [ _, afterApos ] ->
                        if String.startsWith "(" afterApos then
                            parseCardRef afterApos
                                |> Maybe.map
                                    (\card ->
                                        MovedToHand
                                            { player = mover
                                            , card = Just card
                                            , count = Nothing
                                            }
                                    )

                        else
                            let
                                countStr =
                                    afterApos
                                        |> String.split " card"
                                        |> List.head
                                        |> Maybe.withDefault ""
                                        |> String.trim
                            in
                            case String.toInt countStr of
                                Just n ->
                                    Just (MovedToHand { player = mover, card = Nothing, count = Just n })

                                Nothing ->
                                    Nothing

                    _ ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


tryMovedToDiscard : String -> Maybe Action
tryMovedToDiscard raw =
    -- "PLAYER moved PLAYER's N cards to the discard pile."
    if String.contains " moved " raw && String.endsWith " to the discard pile." raw then
        case String.split " moved " raw of
            [ mover, rest ] ->
                case String.split "'s " rest of
                    [ ownerName, afterApos ] ->
                        let
                            countStr =
                                afterApos
                                    |> String.split " card"
                                    |> List.head
                                    |> Maybe.withDefault ""
                                    |> String.trim
                        in
                        case String.toInt countStr of
                            Just n ->
                                Just (MovedToDiscard { mover = mover, owner = ownerName, count = n })

                            Nothing ->
                                Nothing

                    _ ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


tryCoinFlipResult : String -> Maybe Action
tryCoinFlipResult raw =
    -- "PLAYER flipped a coin and it landed on heads/tails."
    -- "PLAYER flipped N coins, and N landed on heads."
    if String.contains " flipped " raw then
        case String.split " flipped " raw of
            [ player, rest ] ->
                if String.startsWith "a coin and it landed on " rest then
                    let
                        outcome =
                            rest
                                |> String.replace "a coin and it landed on " ""
                                |> String.replace "." ""
                                |> String.trim
                    in
                    Just
                        (CoinFlipResult
                            { player = player
                            , flipped = 1
                            , heads =
                                if outcome == "heads" then
                                    1

                                else
                                    0
                            }
                        )

                else
                    -- "N coins, and N landed on heads."
                    case String.split " coins, and " rest of
                        [ flippedStr, headsStr ] ->
                            case ( String.toInt (String.trim flippedStr), headsStr |> String.split " landed" |> List.head |> Maybe.andThen String.toInt ) of
                                ( Just flipped, Just heads ) ->
                                    Just (CoinFlipResult { player = player, flipped = flipped, heads = heads })

                                _ ->
                                    Nothing

                        _ ->
                            Nothing

            _ ->
                Nothing

    else
        Nothing


tryChoseOption : String -> Maybe Action
tryChoseOption raw =
    -- Detail: "PLAYER chose OPTION_NAME"
    if String.contains " chose " raw && not (String.contains " for the opening coin flip" raw) then
        case String.split " chose " raw of
            [ player, option ] ->
                Just (ChoseOption { player = player, option = String.trimRight option })

            _ ->
                Nothing

    else
        Nothing


tryTimeout : String -> Maybe Action
tryTimeout raw =
    -- "PLAYER didn't take an action in time."
    if String.endsWith " didn't take an action in time." raw then
        let
            player =
                String.dropRight 31 raw
        in
        if String.isEmpty player then
            Nothing

        else
            Just (Timeout { player = player })

    else
        Nothing


tryCardList : String -> Maybe Action
tryCardList raw =
    let
        parts =
            if String.startsWith "(" raw then
                raw
                    |> String.split ", ("
                    |> List.indexedMap (\i s -> if i == 0 then s else "(" ++ s)
            else
                []

        cards =
            List.filterMap parseCardRef parts
    in
    if List.length cards >= 2 && List.length cards == List.length parts then
        Just (CardList cards)
    else
        Nothing


-- PRIMITIVE PARSERS


{-| Parse "(id) Name..." → CardRef, ignoring trailing text after the name.
The name extraction mirrors extractCardName in Main.elm / ValidateFixtures.elm.
-}
parseCardRef : String -> Maybe CardRef
parseCardRef str =
    let
        trimmed =
            String.trim str
    in
    if String.startsWith "(" trimmed then
        case String.split ")" trimmed of
            id :: rest ->
                let
                    rawId =
                        String.dropLeft 1 id

                    remainder =
                        String.join ")" rest

                    name =
                        extractFirstName (String.trimLeft remainder)
                in
                if isCardId rawId then
                    Just { id = rawId, name = name }

                else
                    Nothing

            _ ->
                Nothing

    else
        Nothing


{-| Parse "PLAYER's (id) Name" → PokemonRef -}
parsePokemonRef : String -> Maybe PokemonRef
parsePokemonRef str =
    let
        trimmed =
            String.trim str
    in
    case String.split "'s (" trimmed of
        [ player, rest ] ->
            case String.split ")" rest of
                id :: nameParts ->
                    let
                        rawId =
                            id

                        name =
                            String.join ")" nameParts |> String.trimLeft |> extractFirstName
                    in
                    if isCardId rawId then
                        Just { player = player, card = { id = rawId, name = name } }

                    else
                        Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


isCardId : String -> Bool
isCardId s =
    not (String.isEmpty s)
        && String.contains "_" s
        && String.all (\c -> Char.isAlpha c || Char.isDigit c || c == '_' || c == '-') s


{-| Extract the leading card name words from a string (Title Case / "ex" heuristic).
Simplified version: collect words until we hit a non-name token.
-}
extractFirstName : String -> String
extractFirstName content =
    content
        |> String.words
        |> collectNameWords
        |> String.join " "


collectNameWords : List String -> List String
collectNameWords words =
    case words of
        [] ->
            []

        word :: rest ->
            let
                stripped =
                    stripPunct word
            in
            if stripped == "ex" then
                [ "ex" ]

            else if isNameToken stripped then
                stripped :: collectNameWords rest

            else if isConnector word then
                case rest of
                    nextWord :: _ ->
                        if isNameToken (stripPunct nextWord) then
                            word :: collectNameWords rest

                        else
                            []

                    [] ->
                        []

            else
                []


isConnector : String -> Bool
isConnector word =
    word == "of" || word == "at"


isNameToken : String -> Bool
isNameToken word =
    word == "ex"
        || (case String.uncons word of
                Nothing ->
                    False

                Just ( c, _ ) ->
                    Char.isUpper c
                        && not (hasCamelCase word)
           )


hasCamelCase : String -> Bool
hasCamelCase word =
    Tuple.second
        (List.foldl
            (\c ( prevLower, found ) ->
                ( Char.isLower c, found || (Char.isUpper c && prevLower) )
            )
            ( False, False )
            (String.toList word)
        )


stripPunct : String -> String
stripPunct w =
    if String.endsWith "," w || String.endsWith "." w then
        String.dropRight 1 w

    else
        w
