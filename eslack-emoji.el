;;; eslack-emoji.el --- emojis for eslack            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defvar eslack--emoji-alist
  '((:raised-hand:
     (unicode . "✋")
     (image . "raised_hand.png")
     (style . "github"))
    (:trumpet:
     (unicode . "🎺")
     (image . "trumpet.png")
     (style . "github"))
    (:flag-jo:
     (unicode . "🇯🇴")
     (image . "flag_jo.png")
     (style . "github"))
    (:pray_tone3:
     (unicode . "🙏🏽")
     (image . "pray_tone3.png")
     (style . "github"))
    (:imp:
     (unicode . "👿")
     (image . "imp.png")
     (style . "github"))
    (:flag-au:
     (unicode . "🇦🇺")
     (image . "flag_au.png")
     (style . "github"))
    (:reversed_hand_with_middle_finger_extended_tone3:
     (unicode . "🖕🏽")
     (image . "middle_finger_tone3.png")
     (style . "github"))
    (:seat:
     (unicode . "💺")
     (image . "seat.png")
     (style . "github"))
    (:compression:
     (unicode . "🗜")
     (image . "compression.png")
     (style . "github"))
    (:flag_se:
     (unicode . "🇸🇪")
     (image . "flag_se.png")
     (style . "github"))
    (:flag_gb:
     (unicode . "🇬🇧")
     (image . "flag_gb.png")
     (style . "github"))
    (:notebook_with_decorative_cover:
     (unicode . "📔")
     (image . "notebook_with_decorative_cover.png")
     (style . "github"))
    (:black-joker:
     (unicode . "🃏")
     (image . "black_joker.png")
     (style . "github"))
    (:large_blue_diamond:
     (unicode . "🔷")
     (image . "large_blue_diamond.png")
     (style . "github"))
    (:mountain_bicyclist_tone3:
     (unicode . "🚵🏽")
     (image . "mountain_bicyclist_tone3.png")
     (style . "github"))
    (:smiling-imp:
     (unicode . "😈")
     (image . "smiling_imp.png")
     (style . "github"))
    (:musical-score:
     (unicode . "🎼")
     (image . "musical_score.png")
     (style . "github"))
    (:gun:
     (unicode . "🔫")
     (image . "gun.png")
     (style . "github"))
    (:timer_clock:
     (unicode . "⏲")
     (image . "timer.png")
     (style . "github"))
    (:nerd:
     (unicode . "🤓")
     (image . "nerd.png")
     (style . "github"))
    (:poultry_leg:
     (unicode . "🍗")
     (image . "poultry_leg.png")
     (style . "github"))
    (:flag-gy:
     (unicode . "🇬🇾")
     (image . "flag_gy.png")
     (style . "github"))
    (:frame_with_picture:
     (unicode . "🖼")
     (image . "frame_photo.png")
     (style . "github"))
    (:white_circle:
     (unicode . "⚪")
     (image . "white_circle.png")
     (style . "github"))
    (:surfer_tone2:
     (unicode . "🏄🏼")
     (image . "surfer_tone2.png")
     (style . "github"))
    (:point-right-tone4:
     (unicode . "👉🏾")
     (image . "point_right_tone4.png")
     (style . "github"))
    (:heavy_heart_exclamation_mark_ornament:
     (unicode . "❣")
     (image . "heart_exclamation.png")
     (style . "github"))
    (:man_with_turban_tone1:
     (unicode . "👳🏻")
     (image . "man_with_turban_tone1.png")
     (style . "github"))
    (:bw:
     (unicode . "🇧🇼")
     (image . "flag_bw.png")
     (style . "github"))
    (:ug:
     (unicode . "🇺🇬")
     (image . "flag_ug.png")
     (style . "github"))
    (:cloud_with_lightning:
     (unicode . "🌩")
     (image . "cloud_lightning.png")
     (style . "github"))
    (:haircut_tone4:
     (unicode . "💇🏾")
     (image . "haircut_tone4.png")
     (style . "github"))
    (:gn:
     (unicode . "🇬🇳")
     (image . "flag_gn.png")
     (style . "github"))
    (:kissing_cat:
     (unicode . "😽")
     (image . "kissing_cat.png")
     (style . "github"))
    (:flag-sb:
     (unicode . "🇸🇧")
     (image . "flag_sb.png")
     (style . "github"))
    (:heavy_dollar_sign:
     (unicode . "💲")
     (image . "heavy_dollar_sign.png")
     (style . "github"))
    (:flag-gg:
     (unicode . "🇬🇬")
     (image . "flag_gg.png")
     (style . "github"))
    (:open_hands_tone5:
     (unicode . "👐🏿")
     (image . "open_hands_tone5.png")
     (style . "github"))
    (:flag-me:
     (unicode . "🇲🇪")
     (image . "flag_me.png")
     (style . "github"))
    (:point_left:
     (unicode . "👈")
     (image . "point_left.png")
     (style . "github"))
    (:in:
     (unicode . "🇮🇳")
     (image . "flag_in.png")
     (style . "github"))
    (:vertical-traffic-light:
     (unicode . "🚦")
     (image . "vertical_traffic_light.png")
     (style . "github"))
    (:last_quarter_moon:
     (unicode . "🌗")
     (image . "last_quarter_moon.png")
     (style . "github"))
    (:flag-ba:
     (unicode . "🇧🇦")
     (image . "flag_ba.png")
     (style . "github"))
    (:ant:
     (unicode . "🐜")
     (image . "ant.png")
     (style . "github"))
    (:raising_hand:
     (unicode . "🙋")
     (image . "raising_hand.png")
     (style . "github"))
    (:punch_tone5:
     (unicode . "👊🏿")
     (image . "punch_tone5.png")
     (style . "github"))
    (:motorcycle:
     (unicode . "🏍")
     (image . "motorcycle.png")
     (style . "github"))
    (:virgo:
     (unicode . "♍")
     (image . "virgo.png")
     (style . "github"))
    (:telephone_receiver:
     (unicode . "📞")
     (image . "telephone_receiver.png")
     (style . "github"))
    (:runner-tone5:
     (unicode . "🏃🏿")
     (image . "runner_tone5.png")
     (style . "github"))
    (:hatching_chick:
     (unicode . "🐣")
     (image . "hatching_chick.png")
     (style . "github"))
    (:family_wwbb:
     (unicode . "👩👩👦👦")
     (image . "family_wwbb.png")
     (style . "github"))
    (:older_man_tone5:
     (unicode . "👴🏿")
     (image . "older_man_tone5.png")
     (style . "github"))
    (:flag-mw:
     (unicode . "🇲🇼")
     (image . "flag_mw.png")
     (style . "github"))
    (:boy:
     (unicode . "👦")
     (image . "boy.png")
     (style . "github"))
    (:footprints:
     (unicode . "👣")
     (image . "footprints.png")
     (style . "github"))
    (:flag_ma:
     (unicode . "🇲🇦")
     (image . "flag_ma.png")
     (style . "github"))
    (:large-orange-diamond:
     (unicode . "🔶")
     (image . "large_orange_diamond.png")
     (style . "github"))
    (:flag_kp:
     (unicode . "🇰🇵")
     (image . "flag_kp.png")
     (style . "github"))
    (:kimono:
     (unicode . "👘")
     (image . "kimono.png")
     (style . "github"))
    (:kissing_heart:
     (ascii . ":*")
     (unicode . "😘")
     (image . "kissing_heart.png")
     (style . "github"))
    (:newspaper:
     (unicode . "📰")
     (image . "newspaper.png")
     (style . "github"))
    (:person-with-pouting-face-tone2:
     (unicode . "🙎🏼")
     (image . "person_with_pouting_face_tone2.png")
     (style . "github"))
    (:egg:
     (unicode . "🍳")
     (image . "egg.png")
     (style . "github"))
    (:ping-pong:
     (unicode . "🏓")
     (image . "ping_pong.png")
     (style . "github"))
    (:aquarius:
     (unicode . "♒")
     (image . "aquarius.png")
     (style . "github"))
    (:flag_bz:
     (unicode . "🇧🇿")
     (image . "flag_bz.png")
     (style . "github"))
    (:yellow-heart:
     (unicode . "💛")
     (image . "yellow_heart.png")
     (style . "github"))
    (:fire_engine:
     (unicode . "🚒")
     (image . "fire_engine.png")
     (style . "github"))
    (:motorboat:
     (unicode . "🛥")
     (image . "motorboat.png")
     (style . "github"))
    (:flag_al:
     (unicode . "🇦🇱")
     (image . "flag_al.png")
     (style . "github"))
    (:no_pedestrians:
     (unicode . "🚷")
     (image . "no_pedestrians.png")
     (style . "github"))
    (:flag_tr:
     (unicode . "🇹🇷")
     (image . "flag_tr.png")
     (style . "github"))
    (:ok-woman-tone2:
     (unicode . "🙆🏼")
     (image . "ok_woman_tone2.png")
     (style . "github"))
    (:older-woman-tone1:
     (unicode . "👵🏻")
     (image . "older_woman_tone1.png")
     (style . "github"))
    (:middle_finger_tone2:
     (unicode . "🖕🏼")
     (image . "middle_finger_tone2.png")
     (style . "github"))
    (:flag_cn:
     (unicode . "🇨🇳")
     (image . "flag_cn.png")
     (style . "github"))
    (:jm:
     (unicode . "🇯🇲")
     (image . "flag_jm.png")
     (style . "github"))
    (:menorah:
     (unicode . "🕎")
     (image . "menorah.png")
     (style . "github"))
    (:santa_tone1:
     (unicode . "🎅🏻")
     (image . "santa_tone1.png")
     (style . "github"))
    (:diamond_shape_with_a_dot_inside:
     (unicode . "💠")
     (image . "diamond_shape_with_a_dot_inside.png")
     (style . "github"))
    (:black_medium_small_square:
     (unicode . "◾")
     (image . "black_medium_small_square.png")
     (style . "github"))
    (:pk:
     (unicode . "🇵🇰")
     (image . "flag_pk.png")
     (style . "github"))
    (:pear:
     (unicode . "🍐")
     (image . "pear.png")
     (style . "github"))
    (:la:
     (unicode . "🇱🇦")
     (image . "flag_la.png")
     (style . "github"))
    (:walking-tone1:
     (unicode . "🚶🏻")
     (image . "walking_tone1.png")
     (style . "github"))
    (:clock11:
     (unicode . "🕚")
     (image . "clock11.png")
     (style . "github"))
    (:neutral_face:
     (unicode . "😐")
     (image . "neutral_face.png")
     (style . "github"))
    (:repeat:
     (unicode . "🔁")
     (image . "repeat.png")
     (style . "github"))
    (:clock6:
     (unicode . "🕕")
     (image . "clock6.png")
     (style . "github"))
    (:derelict_house_building:
     (unicode . "🏚")
     (image . "house_abandoned.png")
     (style . "github"))
    (:flag_io:
     (unicode . "🇮🇴")
     (image . "flag_io.png")
     (style . "github"))
    (:rocket:
     (unicode . "🚀")
     (image . "rocket.png")
     (style . "github"))
    (:ear_tone5:
     (unicode . "👂🏿")
     (image . "ear_tone5.png")
     (style . "github"))
    (:hash:
     (unicode . "#⃣")
     (image . "hash.png")
     (style . "github"))
    (:-\#
     (unicode . "😶")
     (image . "no_mouth.png")
     (style . "ascii"))
    (:-/
     (unicode . "😕")
     (image . "confused.png")
     (style . "ascii"))
    (:-\.
     (unicode . "😕")
     (image . "confused.png")
     (style . "ascii"))
    (:-\)
     (unicode . "😄")
     (image . "smile.png")
     (style . "ascii"))
    (:tone3:
     (unicode . "🏽")
     (image . "tone3.png")
     (style . "github"))
    (:candle:
     (unicode . "🕯")
     (image . "candle.png")
     (style . "github"))
    (:-*
     (unicode . "😘")
     (image . "kissing_heart.png")
     (style . "ascii"))
    (:flag_mo:
     (unicode . "🇲🇴")
     (image . "flag_mo.png")
     (style . "github"))
    (:person-frowning-tone1:
     (unicode . "🙍🏻")
     (image . "person_frowning_tone1.png")
     (style . "github"))
    (:ship:
     (unicode . "🚢")
     (image . "ship.png")
     (style . "github"))
    (:rosette:
     (unicode . "🏵")
     (image . "rosette.png")
     (style . "github"))
    (:white-sun-rain-cloud:
     (unicode . "🌦")
     (image . "white_sun_rain_cloud.png")
     (style . "github"))
    (:sn:
     (unicode . "🇸🇳")
     (image . "flag_sn.png")
     (style . "github"))
    (:metal-tone1:
     (unicode . "🤘🏻")
     (image . "metal_tone1.png")
     (style . "github"))
    (:bm:
     (unicode . "🇧🇲")
     (image . "flag_bm.png")
     (style . "github"))
    (:flag-ic:
     (unicode . "🇮🇨")
     (image . "flag_ic.png")
     (style . "github"))
    (:raised_hand_with_part_between_middle_and_ring_fingers_tone5:
     (unicode . "🖖🏿")
     (image . "vulcan_tone5.png")
     (style . "github"))
    (:earth_americas:
     (unicode . "🌎")
     (image . "earth_americas.png")
     (style . "github"))
    (:nail-care-tone5:
     (unicode . "💅🏿")
     (image . "nail_care_tone5.png")
     (style . "github"))
    (:point_right_tone1:
     (unicode . "👉🏻")
     (image . "point_right_tone1.png")
     (style . "github"))
    (:chart-with-downwards-trend:
     (unicode . "📉")
     (image . "chart_with_downwards_trend.png")
     (style . "github"))
    (:ping_pong:
     (unicode . "🏓")
     (image . "ping_pong.png")
     (style . "github"))
    (:bs:
     (unicode . "🇧🇸")
     (image . "flag_bs.png")
     (style . "github"))
    (:-p
     (unicode . "😛")
     (image . "stuck_out_tongue.png")
     (style . "ascii"))
    (:construction_worker_tone3:
     (unicode . "👷🏽")
     (image . "construction_worker_tone3.png")
     (style . "github"))
    (:sparkling-heart:
     (unicode . "💖")
     (image . "sparkling_heart.png")
     (style . "github"))
    (:carousel_horse:
     (unicode . "🎠")
     (image . "carousel_horse.png")
     (style . "github"))
    (:basketball-player-tone4:
     (unicode . "⛹🏾")
     (image . "basketball_player_tone4.png")
     (style . "github"))
    (:lips:
     (unicode . "👄")
     (image . "lips.png")
     (style . "github"))
    (:sleuth_or_spy_tone5:
     (unicode . "🕵🏿")
     (image . "spy_tone5.png")
     (style . "github"))
    (:spades:
     (unicode . "♠")
     (image . "spades.png")
     (style . "github"))
    (:angel-tone5:
     (unicode . "👼🏿")
     (image . "angel_tone5.png")
     (style . "github"))
    (:white_sun_small_cloud:
     (unicode . "🌤")
     (image . "white_sun_small_cloud.png")
     (style . "github"))
    (:meat-on-bone:
     (unicode . "🍖")
     (image . "meat_on_bone.png")
     (style . "github"))
    (:-b
     (unicode . "😛")
     (image . "stuck_out_tongue.png")
     (style . "ascii"))
    (:-o
     (unicode . "😮")
     (image . "open_mouth.png")
     (style . "ascii"))
    (:flag_pl:
     (unicode . "🇵🇱")
     (image . "flag_pl.png")
     (style . "github"))
    (:eyeglasses:
     (unicode . "👓")
     (image . "eyeglasses.png")
     (style . "github"))
    (:purple_heart:
     (unicode . "💜")
     (image . "purple_heart.png")
     (style . "github"))
    (:-P
     (unicode . "😛")
     (image . "stuck_out_tongue.png")
     (style . "ascii"))
    (:lifter_tone2:
     (unicode . "🏋🏼")
     (image . "lifter_tone2.png")
     (style . "github"))
    (:full_moon:
     (unicode . "🌕")
     (image . "full_moon.png")
     (style . "github"))
    (:nose_tone5:
     (unicode . "👃🏿")
     (image . "nose_tone5.png")
     (style . "github"))
    (:swimmer-tone4:
     (unicode . "🏊🏾")
     (image . "swimmer_tone4.png")
     (style . "github"))
    (:oncoming-police-car:
     (unicode . "🚔")
     (image . "oncoming_police_car.png")
     (style . "github"))
    (:-X
     (unicode . "😶")
     (image . "no_mouth.png")
     (style . "ascii"))
    (:-\[
     (unicode . "😞")
     (image . "disappointed.png")
     (style . "ascii"))
    (:hand_splayed_tone3:
     (unicode . "🖐🏽")
     (image . "hand_splayed_tone3.png")
     (style . "github"))
    (:nose-tone1:
     (unicode . "👃🏻")
     (image . "nose_tone1.png")
     (style . "github"))
    (:woman-tone2:
     (unicode . "👩🏼")
     (image . "woman_tone2.png")
     (style . "github"))
    (:white-flower:
     (unicode . "💮")
     (image . "white_flower.png")
     (style . "github"))
    (:-O
     (unicode . "😮")
     (image . "open_mouth.png")
     (style . "ascii"))
    (:art:
     (unicode . "🎨")
     (image . "art.png")
     (style . "github"))
    (:shopping-bags:
     (unicode . "🛍")
     (image . "shopping_bags.png")
     (style . "github"))
    (:video_game:
     (unicode . "🎮")
     (image . "video_game.png")
     (style . "github"))
    (:point_down_tone5:
     (unicode . "👇🏿")
     (image . "point_down_tone5.png")
     (style . "github"))
    (:flag-pl:
     (unicode . "🇵🇱")
     (image . "flag_pl.png")
     (style . "github"))
    (:lt:
     (unicode . "🇱🇹")
     (image . "flag_lt.png")
     (style . "github"))
    (:flag_sx:
     (unicode . "🇸🇽")
     (image . "flag_sx.png")
     (style . "github"))
    (:pt:
     (unicode . "🇵🇹")
     (image . "flag_pt.png")
     (style . "github"))
    (:volcano:
     (unicode . "🌋")
     (image . "volcano.png")
     (style . "github"))
    (:ee:
     (unicode . "🇪🇪")
     (image . "flag_ee.png")
     (style . "github"))
    (:arrow_lower_right:
     (unicode . "↘")
     (image . "arrow_lower_right.png")
     (style . "github"))
    (:ear-tone4:
     (unicode . "👂🏾")
     (image . "ear_tone4.png")
     (style . "github"))
    (:meat_on_bone:
     (unicode . "🍖")
     (image . "meat_on_bone.png")
     (style . "github"))
    (:fi:
     (unicode . "🇫🇮")
     (image . "flag_fi.png")
     (style . "github"))
    (:lu:
     (unicode . "🇱🇺")
     (image . "flag_lu.png")
     (style . "github"))
    (:v_tone2:
     (unicode . "✌🏼")
     (image . "v_tone2.png")
     (style . "github"))
    (:girl:
     (unicode . "👧")
     (image . "girl.png")
     (style . "github"))
    (:muscle-tone5:
     (unicode . "💪🏿")
     (image . "muscle_tone5.png")
     (style . "github"))
    (:open-hands-tone1:
     (unicode . "👐🏻")
     (image . "open_hands_tone1.png")
     (style . "github"))
    (:older-man-tone3:
     (unicode . "👴🏽")
     (image . "older_man_tone3.png")
     (style . "github"))
    (:card_file_box:
     (unicode . "🗃")
     (image . "card_box.png")
     (style . "github"))
    (:heart-exclamation:
     (unicode . "❣")
     (image . "heart_exclamation.png")
     (style . "github"))
    (:thinking:
     (unicode . "🤔")
     (image . "thinking.png")
     (style . "github"))
    (:black_circle:
     (unicode . "⚫")
     (image . "black_circle.png")
     (style . "github"))
    (:flag_ky:
     (unicode . "🇰🇾")
     (image . "flag_ky.png")
     (style . "github"))
    (:tt:
     (unicode . "🇹🇹")
     (image . "flag_tt.png")
     (style . "github"))
    (:white-square-button:
     (unicode . "🔳")
     (image . "white_square_button.png")
     (style . "github"))
    (:raised-hands-tone2:
     (unicode . "🙌🏼")
     (image . "raised_hands_tone2.png")
     (style . "github"))
    (:chains:
     (unicode . "⛓")
     (image . "chains.png")
     (style . "github"))
    (:disappointed:
     (ascii . ">:[")
     (unicode . "😞")
     (image . "disappointed.png")
     (style . "github"))
    (:guardsman_tone2:
     (unicode . "💂🏼")
     (image . "guardsman_tone2.png")
     (style . "github"))
    (:shopping_bags:
     (unicode . "🛍")
     (image . "shopping_bags.png")
     (style . "github"))
    (:flag_ls:
     (unicode . "🇱🇸")
     (image . "flag_ls.png")
     (style . "github"))
    (:basketball_player_tone3:
     (unicode . "⛹🏽")
     (image . "basketball_player_tone3.png")
     (style . "github"))
    (:santa-tone3:
     (unicode . "🎅🏽")
     (image . "santa_tone3.png")
     (style . "github"))
    (:raised_hand_with_fingers_splayed_tone2:
     (unicode . "🖐🏼")
     (image . "hand_splayed_tone2.png")
     (style . "github"))
    (:-þ
     (unicode . "😛")
     (image . "stuck_out_tongue.png")
     (style . "ascii"))
    (:metal_tone2:
     (unicode . "🤘🏼")
     (image . "metal_tone2.png")
     (style . "github"))
    (:ca:
     (unicode . "🇨🇦")
     (image . "flag_ca.png")
     (style . "github"))
    (:flag_is:
     (unicode . "🇮🇸")
     (image . "flag_is.png")
     (style . "github"))
    (:grey-exclamation:
     (unicode . "❕")
     (image . "grey_exclamation.png")
     (style . "github"))
    (:clock7:
     (unicode . "🕖")
     (image . "clock7.png")
     (style . "github"))
    (:flag-kg:
     (unicode . "🇰🇬")
     (image . "flag_kg.png")
     (style . "github"))
    (:clock1030:
     (unicode . "🕥")
     (image . "clock1030.png")
     (style . "github"))
    (:sweat_smile:
     (ascii . "':)")
     (unicode . "😅")
     (image . "sweat_smile.png")
     (style . "github"))
    (:closed_lock_with_key:
     (unicode . "🔐")
     (image . "closed_lock_with_key.png")
     (style . "github"))
    (:stopwatch:
     (unicode . "⏱")
     (image . "stopwatch.png")
     (style . "github"))
    (:m:
     (unicode . "Ⓜ")
     (image . "m.png")
     (style . "github"))
    (:cake:
     (unicode . "🍰")
     (image . "cake.png")
     (style . "github"))
    (:pig_nose:
     (unicode . "🐽")
     (image . "pig_nose.png")
     (style . "github"))
    (:bed:
     (unicode . "🛏")
     (image . "bed.png")
     (style . "github"))
    (:turtle:
     (unicode . "🐢")
     (image . "turtle.png")
     (style . "github"))
    (:-Þ
     (unicode . "😛")
     (image . "stuck_out_tongue.png")
     (style . "ascii"))
    (:field-hockey:
     (unicode . "🏑")
     (image . "field_hockey.png")
     (style . "github"))
    (:speaker:
     (unicode . "🔈")
     (image . "speaker.png")
     (style . "github"))
    (:no-smoking:
     (unicode . "🚭")
     (image . "no_smoking.png")
     (style . "github"))
    (:wave:
     (unicode . "👋")
     (image . "wave.png")
     (style . "github"))
    (:person-with-blond-hair-tone3:
     (unicode . "👱🏽")
     (image . "person_with_blond_hair_tone3.png")
     (style . "github"))
    (:flag-mh:
     (unicode . "🇲🇭")
     (image . "flag_mh.png")
     (style . "github"))
    (:desktop:
     (unicode . "🖥")
     (image . "desktop.png")
     (style . "github"))
    (:heavy_minus_sign:
     (unicode . "➖")
     (image . "heavy_minus_sign.png")
     (style . "github"))
    (:wave-tone4:
     (unicode . "👋🏾")
     (image . "wave_tone4.png")
     (style . "github"))
    (:mc:
     (unicode . "🇲🇨")
     (image . "flag_mc.png")
     (style . "github"))
    (:waving_black_flag:
     (unicode . "🏴")
     (image . "flag_black.png")
     (style . "github"))
    (:flag_mm:
     (unicode . "🇲🇲")
     (image . "flag_mm.png")
     (style . "github"))
    (:cloud_with_tornado:
     (unicode . "🌪")
     (image . "cloud_tornado.png")
     (style . "github"))
    (:raising-hand-tone1:
     (unicode . "🙋🏻")
     (image . "raising_hand_tone1.png")
     (style . "github"))
    (:tophat:
     (unicode . "🎩")
     (image . "tophat.png")
     (style . "github"))
    (:spy-tone3:
     (unicode . "🕵🏽")
     (image . "spy_tone3.png")
     (style . "github"))
    (:grandma_tone2:
     (unicode . "👵🏼")
     (image . "older_woman_tone2.png")
     (style . "github"))
    (:lion_face:
     (unicode . "🦁")
     (image . "lion_face.png")
     (style . "github"))
    (:construction_worker:
     (unicode . "👷")
     (image . "construction_worker.png")
     (style . "github"))
    (:triumph:
     (unicode . "😤")
     (image . "triumph.png")
     (style . "github"))
    (:person_frowning_tone3:
     (unicode . "🙍🏽")
     (image . "person_frowning_tone3.png")
     (style . "github"))
    (:ck:
     (unicode . "🇨🇰")
     (image . "flag_ck.png")
     (style . "github"))
    (:flag_ne:
     (unicode . "🇳🇪")
     (image . "flag_ne.png")
     (style . "github"))
    (:mq:
     (unicode . "🇲🇶")
     (image . "flag_mq.png")
     (style . "github"))
    (:dg:
     (unicode . "🇩🇬")
     (image . "flag_dg.png")
     (style . "github"))
    (:flag_gs:
     (unicode . "🇬🇸")
     (image . "flag_gs.png")
     (style . "github"))
    (:ao:
     (unicode . "🇦🇴")
     (image . "flag_ao.png")
     (style . "github"))
    (:raised_hands_tone2:
     (unicode . "🙌🏼")
     (image . "raised_hands_tone2.png")
     (style . "github"))
    (:package:
     (unicode . "📦")
     (image . "package.png")
     (style . "github"))
    (:thumbsup-tone2:
     (unicode . "👍🏼")
     (image . "thumbsup_tone2.png")
     (style . "github"))
    (:pf:
     (unicode . "🇵🇫")
     (image . "flag_pf.png")
     (style . "github"))
    (:es:
     (unicode . "🇪🇸")
     (image . "flag_es.png")
     (style . "github"))
    (:point-up-tone3:
     (unicode . "☝🏽")
     (image . "point_up_tone3.png")
     (style . "github"))
    (:flag_sj:
     (unicode . "🇸🇯")
     (image . "flag_sj.png")
     (style . "github"))
    (:flag-eg:
     (unicode . "🇪🇬")
     (image . "flag_eg.png")
     (style . "github"))
    (:pray_tone4:
     (unicode . "🙏🏾")
     (image . "pray_tone4.png")
     (style . "github"))
    (:chart:
     (unicode . "💹")
     (image . "chart.png")
     (style . "github"))
    (:joy-cat:
     (unicode . "😹")
     (image . "joy_cat.png")
     (style . "github"))
    (:a:
     (unicode . "🅰")
     (image . "a.png")
     (style . "github"))
    (:flag-at:
     (unicode . "🇦🇹")
     (image . "flag_at.png")
     (style . "github"))
    (:kissing-smiling-eyes:
     (unicode . "😙")
     (image . "kissing_smiling_eyes.png")
     (style . "github"))
    (:angel-tone2:
     (unicode . "👼🏼")
     (image . "angel_tone2.png")
     (style . "github"))
    (:man-with-gua-pi-mao-tone2:
     (unicode . "👲🏼")
     (image . "man_with_gua_pi_mao_tone2.png")
     (style . "github"))
    (:flag_sd:
     (unicode . "🇸🇩")
     (image . "flag_sd.png")
     (style . "github"))
    (:pineapple:
     (unicode . "🍍")
     (image . "pineapple.png")
     (style . "github"))
    (:nf:
     (unicode . "🇳🇫")
     (image . "flag_nf.png")
     (style . "github"))
    (:oncoming_automobile:
     (unicode . "🚘")
     (image . "oncoming_automobile.png")
     (style . "github"))
    (:flag-et:
     (unicode . "🇪🇹")
     (image . "flag_et.png")
     (style . "github"))
    (:thumbsup_tone4:
     (unicode . "👍🏾")
     (image . "thumbsup_tone4.png")
     (style . "github"))
    (:next_track:
     (unicode . "⏭")
     (image . "track_next.png")
     (style . "github"))
    (:construction-worker-tone5:
     (unicode . "👷🏿")
     (image . "construction_worker_tone5.png")
     (style . "github"))
    (:fist_tone1:
     (unicode . "✊🏻")
     (image . "fist_tone1.png")
     (style . "github"))
    (:wavy_dash:
     (unicode . "〰")
     (image . "wavy_dash.png")
     (style . "github"))
    (:flags:
     (unicode . "🎏")
     (image . "flags.png")
     (style . "github"))
    (:writing_hand_tone2:
     (unicode . "✍🏼")
     (image . "writing_hand_tone2.png")
     (style . "github"))
    (:ferris-wheel:
     (unicode . "🎡")
     (image . "ferris_wheel.png")
     (style . "github"))
    (:haircut_tone5:
     (unicode . "💇🏿")
     (image . "haircut_tone5.png")
     (style . "github"))
    (:speaking-head:
     (unicode . "🗣")
     (image . "speaking_head.png")
     (style . "github"))
    (:flag-vc:
     (unicode . "🇻🇨")
     (image . "flag_vc.png")
     (style . "github"))
    (:woman-tone5:
     (unicode . "👩🏿")
     (image . "woman_tone5.png")
     (style . "github"))
    (:skeleton:
     (unicode . "💀")
     (image . "skull.png")
     (style . "github"))
    (:clock8:
     (unicode . "🕗")
     (image . "clock8.png")
     (style . "github"))
    (:point_up_tone1:
     (unicode . "☝🏻")
     (image . "point_up_tone1.png")
     (style . "github"))
    (:pisces:
     (unicode . "♓")
     (image . "pisces.png")
     (style . "github"))
    (:poop:
     (unicode . "💩")
     (image . "poop.png")
     (style . "github"))
    (:e-mail:
     (unicode . "📧")
     (image . "e_mail.png")
     (style . "github"))
    (:us:
     (unicode . "🇺🇸")
     (image . "flag_us.png")
     (style . "github"))
    (:movie-camera:
     (unicode . "🎥")
     (image . "movie_camera.png")
     (style . "github"))
    (:flag-sa:
     (unicode . "🇸🇦")
     (image . "flag_sa.png")
     (style . "github"))
    (:green_apple:
     (unicode . "🍏")
     (image . "green_apple.png")
     (style . "github"))
    (:mailbox-with-no-mail:
     (unicode . "📭")
     (image . "mailbox_with_no_mail.png")
     (style . "github"))
    (:flag_be:
     (unicode . "🇧🇪")
     (image . "flag_be.png")
     (style . "github"))
    (:arrow-heading-up:
     (unicode . "⤴")
     (image . "arrow_heading_up.png")
     (style . "github"))
    (:wc:
     (unicode . "🚾")
     (image . "wc.png")
     (style . "github"))
    (:black-square-button:
     (unicode . "🔲")
     (image . "black_square_button.png")
     (style . "github"))
    (:flag-bb:
     (unicode . "🇧🇧")
     (image . "flag_bb.png")
     (style . "github"))
    (:headphones:
     (unicode . "🎧")
     (image . "headphones.png")
     (style . "github"))
    (:hole:
     (unicode . "🕳")
     (image . "hole.png")
     (style . "github"))
    (:flag_vu:
     (unicode . "🇻🇺")
     (image . "flag_vu.png")
     (style . "github"))
    (:slightly_frowning_face:
     (unicode . "🙁")
     (image . "slight_frown.png")
     (style . "github"))
    (:flag-mv:
     (unicode . "🇲🇻")
     (image . "flag_mv.png")
     (style . "github"))
    (:massage:
     (unicode . "💆")
     (image . "massage.png")
     (style . "github"))
    (:small_red_triangle_down:
     (unicode . "🔻")
     (image . "small_red_triangle_down.png")
     (style . "github"))
    (:flag_dm:
     (unicode . "🇩🇲")
     (image . "flag_dm.png")
     (style . "github"))
    (:ua:
     (unicode . "🇺🇦")
     (image . "flag_ua.png")
     (style . "github"))
    (:flag_ta:
     (unicode . "🇹🇦")
     (image . "flag_ta.png")
     (style . "github"))
    (:tennis:
     (unicode . "🎾")
     (image . "tennis.png")
     (style . "github"))
    (:blue_book:
     (unicode . "📘")
     (image . "blue_book.png")
     (style . "github"))
    (:watermelon:
     (unicode . "🍉")
     (image . "watermelon.png")
     (style . "github"))
    (:raised-hand-tone4:
     (unicode . "✋🏾")
     (image . "raised_hand_tone4.png")
     (style . "github"))
    (:md:
     (unicode . "🇲🇩")
     (image . "flag_md.png")
     (style . "github"))
    (:bangbang:
     (unicode . "‼")
     (image . "bangbang.png")
     (style . "github"))
    (:flag-cv:
     (unicode . "🇨🇻")
     (image . "flag_cv.png")
     (style . "github"))
    (:slight-smile:
     (unicode . "🙂")
     (image . "slight_smile.png")
     (style . "github"))
    (:ax:
     (unicode . "🇦🇽")
     (image . "flag_ax.png")
     (style . "github"))
    (:bellhop_bell:
     (unicode . "🛎")
     (image . "bellhop.png")
     (style . "github"))
    (:point-down-tone3:
     (unicode . "👇🏽")
     (image . "point_down_tone3.png")
     (style . "github"))
    (:frog:
     (unicode . "🐸")
     (image . "frog.png")
     (style . "github"))
    (:printer:
     (unicode . "🖨")
     (image . "printer.png")
     (style . "github"))
    (:writing-hand-tone4:
     (unicode . "✍🏾")
     (image . "writing_hand_tone4.png")
     (style . "github"))
    (:swimmer_tone4:
     (unicode . "🏊🏾")
     (image . "swimmer_tone4.png")
     (style . "github"))
    (:wave_tone3:
     (unicode . "👋🏽")
     (image . "wave_tone3.png")
     (style . "github"))
    (:right_anger_bubble:
     (unicode . "🗯")
     (image . "anger_right.png")
     (style . "github"))
    (:green_heart:
     (unicode . "💚")
     (image . "green_heart.png")
     (style . "github"))
    (:pouting_cat:
     (unicode . "😾")
     (image . "pouting_cat.png")
     (style . "github"))
    (:mr:
     (unicode . "🇲🇷")
     (image . "flag_mr.png")
     (style . "github"))
    (:flag-ta:
     (unicode . "🇹🇦")
     (image . "flag_ta.png")
     (style . "github"))
    (:boar:
     (unicode . "🐗")
     (image . "boar.png")
     (style . "github"))
    (:smirk:
     (unicode . "😏")
     (image . "smirk.png")
     (style . "github"))
    (:middle_finger_tone5:
     (unicode . "🖕🏿")
     (image . "middle_finger_tone5.png")
     (style . "github"))
    (:soccer:
     (unicode . "⚽")
     (image . "soccer.png")
     (style . "github"))
    (:sob:
     (unicode . "😭")
     (image . "sob.png")
     (style . "github"))
    (:baby-tone3:
     (unicode . "👶🏽")
     (image . "baby_tone3.png")
     (style . "github"))
    (:santa:
     (unicode . "🎅")
     (image . "santa.png")
     (style . "github"))
    (:flag-cd:
     (unicode . "🇨🇩")
     (image . "flag_cd.png")
     (style . "github"))
    (:race_car:
     (unicode . "🏎")
     (image . "race_car.png")
     (style . "github"))
    (:star2:
     (unicode . "🌟")
     (image . "star2.png")
     (style . "github"))
    (:construction-worker-tone4:
     (unicode . "👷🏾")
     (image . "construction_worker_tone4.png")
     (style . "github"))
    (:ok-hand:
     (unicode . "👌")
     (image . "ok_hand.png")
     (style . "github"))
    (:sparkling_heart:
     (unicode . "💖")
     (image . "sparkling_heart.png")
     (style . "github"))
    (:princess_tone5:
     (unicode . "👸🏿")
     (image . "princess_tone5.png")
     (style . "github"))
    (:flag-eh:
     (unicode . "🇪🇭")
     (image . "flag_eh.png")
     (style . "github"))
    (:flag_am:
     (unicode . "🇦🇲")
     (image . "flag_am.png")
     (style . "github"))
    (:bicyclist_tone1:
     (unicode . "🚴🏻")
     (image . "bicyclist_tone1.png")
     (style . "github"))
    (:violin:
     (unicode . "🎻")
     (image . "violin.png")
     (style . "github"))
    (:crossed_swords:
     (unicode . "⚔")
     (image . "crossed_swords.png")
     (style . "github"))
    (:clock12:
     (unicode . "🕛")
     (image . "clock12.png")
     (style . "github"))
    (:\(
     (unicode . "😞")
     (image . "disappointed.png")
     (style . "ascii"))
    (:flag_um:
     (unicode . "🇺🇲")
     (image . "flag_um.png")
     (style . "github"))
    (:radioactive_sign:
     (unicode . "☢")
     (image . "radioactive.png")
     (style . "github"))
    (:last_quarter_moon_with_face:
     (unicode . "🌜")
     (image . "last_quarter_moon_with_face.png")
     (style . "github"))
    (:kissing-cat:
     (unicode . "😽")
     (image . "kissing_cat.png")
     (style . "github"))
    (:flag_in:
     (unicode . "🇮🇳")
     (image . "flag_in.png")
     (style . "github"))
    (:boy_tone4:
     (unicode . "👦🏾")
     (image . "boy_tone4.png")
     (style . "github"))
    (:spy_tone2:
     (unicode . "🕵🏼")
     (image . "spy_tone2.png")
     (style . "github"))
    (:love-hotel:
     (unicode . "🏩")
     (image . "love_hotel.png")
     (style . "github"))
    (:bl:
     (unicode . "🇧🇱")
     (image . "flag_bl.png")
     (style . "github"))
    (:haircut-tone4:
     (unicode . "💇🏾")
     (image . "haircut_tone4.png")
     (style . "github"))
    (:lr:
     (unicode . "🇱🇷")
     (image . "flag_lr.png")
     (style . "github"))
    (:chart_with_upwards_trend:
     (unicode . "📈")
     (image . "chart_with_upwards_trend.png")
     (style . "github"))
    (:rowboat-tone3:
     (unicode . "🚣🏽")
     (image . "rowboat_tone3.png")
     (style . "github"))
    (:small-blue-diamond:
     (unicode . "🔹")
     (image . "small_blue_diamond.png")
     (style . "github"))
    (:fishing_pole_and_fish:
     (unicode . "🎣")
     (image . "fishing_pole_and_fish.png")
     (style . "github"))
    (:leftwards_arrow_with_hook:
     (unicode . "↩")
     (image . "leftwards_arrow_with_hook.png")
     (style . "github"))
    (:last-quarter-moon:
     (unicode . "🌗")
     (image . "last_quarter_moon.png")
     (style . "github"))
    (:octopus:
     (unicode . "🐙")
     (image . "octopus.png")
     (style . "github"))
    (:so:
     (unicode . "🇸🇴")
     (image . "flag_so.png")
     (style . "github"))
    (:sparkles:
     (unicode . "✨")
     (image . "sparkles.png")
     (style . "github"))
    (:red_circle:
     (unicode . "🔴")
     (image . "red_circle.png")
     (style . "github"))
    (:na:
     (unicode . "🇳🇦")
     (image . "flag_na.png")
     (style . "github"))
    (:umbrella:
     (unicode . "☔")
     (image . "umbrella.png")
     (style . "github"))
    (:no-pedestrians:
     (unicode . "🚷")
     (image . "no_pedestrians.png")
     (style . "github"))
    (:saudiarabia:
     (unicode . "🇸🇦")
     (image . "flag_sa.png")
     (style . "github"))
    (:flag_vc:
     (unicode . "🇻🇨")
     (image . "flag_vc.png")
     (style . "github"))
    (:angel-tone3:
     (unicode . "👼🏽")
     (image . "angel_tone3.png")
     (style . "github"))
    (:flag_pm:
     (unicode . "🇵🇲")
     (image . "flag_pm.png")
     (style . "github"))
    (:br:
     (unicode . "🇧🇷")
     (image . "flag_br.png")
     (style . "github"))
    (:bj:
     (unicode . "🇧🇯")
     (image . "flag_bj.png")
     (style . "github"))
    (:family-mwgb:
     (unicode . "👨👩👧👦")
     (image . "family_mwgb.png")
     (style . "github"))
    (:arrow-right:
     (unicode . "➡")
     (image . "arrow_right.png")
     (style . "github"))
    (:door:
     (unicode . "🚪")
     (image . "door.png")
     (style . "github"))
    (:girl_tone5:
     (unicode . "👧🏿")
     (image . "girl_tone5.png")
     (style . "github"))
    (:baby:
     (unicode . "👶")
     (image . "baby.png")
     (style . "github"))
    (:nose_tone4:
     (unicode . "👃🏾")
     (image . "nose_tone4.png")
     (style . "github"))
    (:passport_control:
     (unicode . "🛂")
     (image . "passport_control.png")
     (style . "github"))
    (:battery:
     (unicode . "🔋")
     (image . "battery.png")
     (style . "github"))
    (:lifter_tone3:
     (unicode . "🏋🏽")
     (image . "lifter_tone3.png")
     (style . "github"))
    (:family-mmgb:
     (unicode . "👨👨👧👦")
     (image . "family_mmgb.png")
     (style . "github"))
    (:beach_with_umbrella:
     (unicode . "🏖")
     (image . "beach.png")
     (style . "github"))
    (:person_with_pouting_face_tone5:
     (unicode . "🙎🏿")
     (image . "person_with_pouting_face_tone5.png")
     (style . "github"))
    (:flag-zm:
     (unicode . "🇿🇲")
     (image . "flag_zm.png")
     (style . "github"))
    (:mantlepiece_clock:
     (unicode . "🕰")
     (image . "clock.png")
     (style . "github"))
    (:hu:
     (unicode . "🇭🇺")
     (image . "flag_hu.png")
     (style . "github"))
    (:heart-eyes-cat:
     (unicode . "😻")
     (image . "heart_eyes_cat.png")
     (style . "github"))
    (:flag-pm:
     (unicode . "🇵🇲")
     (image . "flag_pm.png")
     (style . "github"))
    (:cop-tone5:
     (unicode . "👮🏿")
     (image . "cop_tone5.png")
     (style . "github"))
    (:mountain-bicyclist-tone4:
     (unicode . "🚵🏾")
     (image . "mountain_bicyclist_tone4.png")
     (style . "github"))
    (:point_up_tone5:
     (unicode . "☝🏿")
     (image . "point_up_tone5.png")
     (style . "github"))
    (:raised-hands-tone1:
     (unicode . "🙌🏻")
     (image . "raised_hands_tone1.png")
     (style . "github"))
    (:confetti-ball:
     (unicode . "🎊")
     (image . "confetti_ball.png")
     (style . "github"))
    (:point_down_tone4:
     (unicode . "👇🏾")
     (image . "point_down_tone4.png")
     (style . "github"))
    (:game_die:
     (unicode . "🎲")
     (image . "game_die.png")
     (style . "github"))
    (:bamboo:
     (unicode . "🎍")
     (image . "bamboo.png")
     (style . "github"))
    (:older-man-tone2:
     (unicode . "👴🏼")
     (image . "older_man_tone2.png")
     (style . "github"))
    (:deciduous-tree:
     (unicode . "🌳")
     (image . "deciduous_tree.png")
     (style . "github"))
    (:scorpius:
     (unicode . "♏")
     (image . "scorpius.png")
     (style . "github"))
    (:tw:
     (unicode . "🇹🇼")
     (image . "flag_tw.png")
     (style . "github"))
    (:arrow-up-down:
     (unicode . "↕")
     (image . "arrow_up_down.png")
     (style . "github"))
    (:control-knobs:
     (unicode . "🎛")
     (image . "control_knobs.png")
     (style . "github"))
    (:angry:
     (ascii . ">:(")
     (unicode . "😠")
     (image . "angry.png")
     (style . "github"))
    (:jp:
     (unicode . "🇯🇵")
     (image . "flag_jp.png")
     (style . "github"))
    (:loud-sound:
     (unicode . "🔊")
     (image . "loud_sound.png")
     (style . "github"))
    (:crescent-moon:
     (unicode . "🌙")
     (image . "crescent_moon.png")
     (style . "github"))
    (:leopard:
     (unicode . "🐆")
     (image . "leopard.png")
     (style . "github"))
    (:raising-hand:
     (unicode . "🙋")
     (image . "raising_hand.png")
     (style . "github"))
    (:syringe:
     (unicode . "💉")
     (image . "syringe.png")
     (style . "github"))
    (:flag-ke:
     (unicode . "🇰🇪")
     (image . "flag_ke.png")
     (style . "github"))
    (:basketball_player_tone2:
     (unicode . "⛹🏼")
     (image . "basketball_player_tone2.png")
     (style . "github"))
    (:medal:
     (unicode . "🏅")
     (image . "medal.png")
     (style . "github"))
    (:man_with_gua_pi_mao_tone1:
     (unicode . "👲🏻")
     (image . "man_with_gua_pi_mao_tone1.png")
     (style . "github"))
    (:þ
     (unicode . "😛")
     (image . "stuck_out_tongue.png")
     (style . "ascii"))
    (:flag_ir:
     (unicode . "🇮🇷")
     (image . "flag_ir.png")
     (style . "github"))
    (:cross:
     (unicode . "✝")
     (image . "cross.png")
     (style . "github"))
    (:flag-bm:
     (unicode . "🇧🇲")
     (image . "flag_bm.png")
     (style . "github"))
    (:joy:
     (ascii . ":')")
     (unicode . "😂")
     (image . "joy.png")
     (style . "github"))
    (:dark-sunglasses:
     (unicode . "🕶")
     (image . "dark_sunglasses.png")
     (style . "github"))
    (:-x
     (unicode . "😶")
     (image . "no_mouth.png")
     (style . "ascii"))
    (:dancer-tone1:
     (unicode . "💃🏻")
     (image . "dancer_tone1.png")
     (style . "github"))
    (:lower_left_crayon:
     (unicode . "🖍")
     (image . "crayon.png")
     (style . "github"))
    (:person-with-blond-hair-tone2:
     (unicode . "👱🏼")
     (image . "person_with_blond_hair_tone2.png")
     (style . "github"))
    (:first-quarter-moon-with-face:
     (unicode . "🌛")
     (image . "first_quarter_moon_with_face.png")
     (style . "github"))
    (:anger_right:
     (unicode . "🗯")
     (image . "anger_right.png")
     (style . "github"))
    (:wave-tone3:
     (unicode . "👋🏽")
     (image . "wave_tone3.png")
     (style . "github"))
    (:kz:
     (unicode . "🇰🇿")
     (image . "flag_kz.png")
     (style . "github"))
    (:eg:
     (unicode . "🇪🇬")
     (image . "flag_eg.png")
     (style . "github"))
    (:melon:
     (unicode . "🍈")
     (image . "melon.png")
     (style . "github"))
    (:hockey:
     (unicode . "🏒")
     (image . "hockey.png")
     (style . "github"))
    (:thumbsup-tone3:
     (unicode . "👍🏽")
     (image . "thumbsup_tone3.png")
     (style . "github"))
    (:point_up_tone2:
     (unicode . "☝🏼")
     (image . "point_up_tone2.png")
     (style . "github"))
    (:hugging_face:
     (unicode . "🤗")
     (image . "hugging.png")
     (style . "github"))
    (:flag-dm:
     (unicode . "🇩🇲")
     (image . "flag_dm.png")
     (style . "github"))
    (:grandma_tone3:
     (unicode . "👵🏽")
     (image . "older_woman_tone3.png")
     (style . "github"))
    (:heavy-division-sign:
     (unicode . "➗")
     (image . "heavy_division_sign.png")
     (style . "github"))
    (:flag-ae:
     (unicode . "🇦🇪")
     (image . "flag_ae.png")
     (style . "github"))
    (:swimmer_tone1:
     (unicode . "🏊🏻")
     (image . "swimmer_tone1.png")
     (style . "github"))
    (:flag-fi:
     (unicode . "🇫🇮")
     (image . "flag_fi.png")
     (style . "github"))
    (:basketball-player:
     (unicode . "⛹")
     (image . "basketball_player.png")
     (style . "github"))
    (:flag_gr:
     (unicode . "🇬🇷")
     (image . "flag_gr.png")
     (style . "github"))
    (:raised_hands_tone1:
     (unicode . "🙌🏻")
     (image . "raised_hands_tone1.png")
     (style . "github"))
    (:nose-tone5:
     (unicode . "👃🏿")
     (image . "nose_tone5.png")
     (style . "github"))
    (:underage:
     (unicode . "🔞")
     (image . "underage.png")
     (style . "github"))
    (:pa:
     (unicode . "🇵🇦")
     (image . "flag_pa.png")
     (style . "github"))
    (:money_with_wings:
     (unicode . "💸")
     (image . "money_with_wings.png")
     (style . "github"))
    (:badminton:
     (unicode . "🏸")
     (image . "badminton.png")
     (style . "github"))
    (:white_sun_rain_cloud:
     (unicode . "🌦")
     (image . "white_sun_rain_cloud.png")
     (style . "github"))
    (:leftwards-arrow-with-hook:
     (unicode . "↩")
     (image . "leftwards_arrow_with_hook.png")
     (style . "github"))
    (:flag_lr:
     (unicode . "🇱🇷")
     (image . "flag_lr.png")
     (style . "github"))
    (:two_hearts:
     (unicode . "💕")
     (image . "two_hearts.png")
     (style . "github"))
    (:flag_gd:
     (unicode . "🇬🇩")
     (image . "flag_gd.png")
     (style . "github"))
    (:link:
     (unicode . "🔗")
     (image . "link.png")
     (style . "github"))
    (:heavy_multiplication_x:
     (unicode . "✖")
     (image . "heavy_multiplication_x.png")
     (style . "github"))
    (:incoming-envelope:
     (unicode . "📨")
     (image . "incoming_envelope.png")
     (style . "github"))
    (:pick:
     (unicode . "⛏")
     (image . "pick.png")
     (style . "github"))
    (:pray_tone5:
     (unicode . "🙏🏿")
     (image . "pray_tone5.png")
     (style . "github"))
    (:dancer:
     (unicode . "💃")
     (image . "dancer.png")
     (style . "github"))
    (:flag_fr:
     (unicode . "🇫🇷")
     (image . "flag_fr.png")
     (style . "github"))
    (:white_flower:
     (unicode . "💮")
     (image . "white_flower.png")
     (style . "github"))
    (:flag_sg:
     (unicode . "🇸🇬")
     (image . "flag_sg.png")
     (style . "github"))
    (:free:
     (unicode . "🆓")
     (image . "free.png")
     (style . "github"))
    (:clock530:
     (unicode . "🕠")
     (image . "clock530.png")
     (style . "github"))
    (:womans-clothes:
     (unicode . "👚")
     (image . "womans_clothes.png")
     (style . "github"))
    (:high-heel:
     (unicode . "👠")
     (image . "high_heel.png")
     (style . "github"))
    (:no_entry_sign:
     (unicode . "🚫")
     (image . "no_entry_sign.png")
     (style . "github"))
    (:revolving-hearts:
     (unicode . "💞")
     (image . "revolving_hearts.png")
     (style . "github"))
    (:ideograph_advantage:
     (unicode . "🉐")
     (image . "ideograph_advantage.png")
     (style . "github"))
    (:green-apple:
     (unicode . "🍏")
     (image . "green_apple.png")
     (style . "github"))
    (:flag-ne:
     (unicode . "🇳🇪")
     (image . "flag_ne.png")
     (style . "github"))
    (:princess-tone3:
     (unicode . "👸🏽")
     (image . "princess_tone3.png")
     (style . "github"))
    (:u5408:
     (unicode . "🈴")
     (image . "u5408.png")
     (style . "github"))
    (:my:
     (unicode . "🇲🇾")
     (image . "flag_my.png")
     (style . "github"))
    (:thumbsup_tone5:
     (unicode . "👍🏿")
     (image . "thumbsup_tone5.png")
     (style . "github"))
    (:point_left_tone4:
     (unicode . "👈🏾")
     (image . "point_left_tone4.png")
     (style . "github"))
    (:flag-gw:
     (unicode . "🇬🇼")
     (image . "flag_gw.png")
     (style . "github"))
    (:runner_tone1:
     (unicode . "🏃🏻")
     (image . "runner_tone1.png")
     (style . "github"))
    (:writing_hand_tone3:
     (unicode . "✍🏽")
     (image . "writing_hand_tone3.png")
     (style . "github"))
    (:clap_tone1:
     (unicode . "👏🏻")
     (image . "clap_tone1.png")
     (style . "github"))
    (:surfer-tone4:
     (unicode . "🏄🏾")
     (image . "surfer_tone4.png")
     (style . "github"))
    (:convenience_store:
     (unicode . "🏪")
     (image . "convenience_store.png")
     (style . "github"))
    (:walking_tone5:
     (unicode . "🚶🏿")
     (image . "walking_tone5.png")
     (style . "github"))
    (:thumbsdown_tone4:
     (unicode . "👎🏾")
     (image . "thumbsdown_tone4.png")
     (style . "github"))
    (:heavy_check_mark:
     (unicode . "✔")
     (image . "heavy_check_mark.png")
     (style . "github"))
    (:wave_tone4:
     (unicode . "👋🏾")
     (image . "wave_tone4.png")
     (style . "github"))
    (:ear-tone2:
     (unicode . "👂🏼")
     (image . "ear_tone2.png")
     (style . "github"))
    (:woman-tone3:
     (unicode . "👩🏽")
     (image . "woman_tone3.png")
     (style . "github"))
    (:flag-mg:
     (unicode . "🇲🇬")
     (image . "flag_mg.png")
     (style . "github"))
    (:gh:
     (unicode . "🇬🇭")
     (image . "flag_gh.png")
     (style . "github"))
    (:mobile-phone-off:
     (unicode . "📴")
     (image . "mobile_phone_off.png")
     (style . "github"))
    (:flower_playing_cards:
     (unicode . "🎴")
     (image . "flower_playing_cards.png")
     (style . "github"))
    (:hammer-pick:
     (unicode . "⚒")
     (image . "hammer_pick.png")
     (style . "github"))
    (:mountain_bicyclist_tone1:
     (unicode . "🚵🏻")
     (image . "mountain_bicyclist_tone1.png")
     (style . "github"))
    (:baby-tone2:
     (unicode . "👶🏼")
     (image . "baby_tone2.png")
     (style . "github"))
    (:fries:
     (unicode . "🍟")
     (image . "fries.png")
     (style . "github"))
    (:il:
     (unicode . "🇮🇱")
     (image . "flag_il.png")
     (style . "github"))
    (:pe:
     (unicode . "🇵🇪")
     (image . "flag_pe.png")
     (style . "github"))
    (:clock:
     (unicode . "🕰")
     (image . "clock.png")
     (style . "github"))
    (:airplane_departure:
     (unicode . "🛫")
     (image . "airplane_departure.png")
     (style . "github"))
    (:flag-aw:
     (unicode . "🇦🇼")
     (image . "flag_aw.png")
     (style . "github"))
    (:beach:
     (unicode . "🏖")
     (image . "beach.png")
     (style . "github"))
    (:arrow-up-small:
     (unicode . "🔼")
     (image . "arrow_up_small.png")
     (style . "github"))
    (:flag_bj:
     (unicode . "🇧🇯")
     (image . "flag_bj.png")
     (style . "github"))
    (:1234:
     (unicode . "🔢")
     (image . "1234.png")
     (style . "github"))
    (:flag-hm:
     (unicode . "🇭🇲")
     (image . "flag_hm.png")
     (style . "github"))
    (:no-good-tone5:
     (unicode . "🙅🏿")
     (image . "no_good_tone5.png")
     (style . "github"))
    (:shield:
     (unicode . "🛡")
     (image . "shield.png")
     (style . "github"))
    (:eight-pointed-black-star:
     (unicode . "✴")
     (image . "eight_pointed_black_star.png")
     (style . "github"))
    (:flag-sv:
     (unicode . "🇸🇻")
     (image . "flag_sv.png")
     (style . "github"))
    (:clock2:
     (unicode . "🕑")
     (image . "clock2.png")
     (style . "github"))
    (:bust_in_silhouette:
     (unicode . "👤")
     (image . "bust_in_silhouette.png")
     (style . "github"))
    (:airplane-departure:
     (unicode . "🛫")
     (image . "airplane_departure.png")
     (style . "github"))
    (:flag-my:
     (unicode . "🇲🇾")
     (image . "flag_my.png")
     (style . "github"))
    (:bread:
     (unicode . "🍞")
     (image . "bread.png")
     (style . "github"))
    (:muscle-tone1:
     (unicode . "💪🏻")
     (image . "muscle_tone1.png")
     (style . "github"))
    (:left-right-arrow:
     (unicode . "↔")
     (image . "left_right_arrow.png")
     (style . "github"))
    (:horse-racing-tone4:
     (unicode . "🏇🏾")
     (image . "horse_racing_tone4.png")
     (style . "github"))
    (:rat:
     (unicode . "🐀")
     (image . "rat.png")
     (style . "github"))
    (:mans_shoe:
     (unicode . "👞")
     (image . "mans_shoe.png")
     (style . "github"))
    (:tulip:
     (unicode . "🌷")
     (image . "tulip.png")
     (style . "github"))
    (:rowboat:
     (unicode . "🚣")
     (image . "rowboat.png")
     (style . "github"))
    (:trackball:
     (unicode . "🖲")
     (image . "trackball.png")
     (style . "github"))
    (:wheelchair:
     (unicode . "♿")
     (image . "wheelchair.png")
     (style . "github"))
    (:put-litter-in-its-place:
     (unicode . "🚮")
     (image . "put_litter_in_its_place.png")
     (style . "github"))
    (:raised-hand-tone5:
     (unicode . "✋🏿")
     (image . "raised_hand_tone5.png")
     (style . "github"))
    (:metal_tone4:
     (unicode . "🤘🏾")
     (image . "metal_tone4.png")
     (style . "github"))
    (:flag-cu:
     (unicode . "🇨🇺")
     (image . "flag_cu.png")
     (style . "github"))
    (:fax:
     (unicode . "📠")
     (image . "fax.png")
     (style . "github"))
    (:blue-book:
     (unicode . "📘")
     (image . "blue_book.png")
     (style . "github"))
    (:monkey_face:
     (unicode . "🐵")
     (image . "monkey_face.png")
     (style . "github"))
    (:writing-hand-tone5:
     (unicode . "✍🏿")
     (image . "writing_hand_tone5.png")
     (style . "github"))
    (:x:
     (unicode . "❌")
     (image . "x.png")
     (style . "github"))
    (:bride-with-veil:
     (unicode . "👰")
     (image . "bride_with_veil.png")
     (style . "github"))
    (:couple_mm:
     (unicode . "👨❤👨")
     (image . "couple_mm.png")
     (style . "github"))
    (:older-woman-tone3:
     (unicode . "👵🏽")
     (image . "older_woman_tone3.png")
     (style . "github"))
    (:flag_bf:
     (unicode . "🇧🇫")
     (image . "flag_bf.png")
     (style . "github"))
    (:middle_finger_tone4:
     (unicode . "🖕🏾")
     (image . "middle_finger_tone4.png")
     (style . "github"))
    (:flag-tn:
     (unicode . "🇹🇳")
     (image . "flag_tn.png")
     (style . "github"))
    (:mountain:
     (unicode . "⛰")
     (image . "mountain.png")
     (style . "github"))
    (:convenience-store:
     (unicode . "🏪")
     (image . "convenience_store.png")
     (style . "github"))
    (:level_slider:
     (unicode . "🎚")
     (image . "level_slider.png")
     (style . "github"))
    (:bride_with_veil_tone3:
     (unicode . "👰🏽")
     (image . "bride_with_veil_tone3.png")
     (style . "github"))
    (:man-tone1:
     (unicode . "👨🏻")
     (image . "man_tone1.png")
     (style . "github"))
    (:flag-ck:
     (unicode . "🇨🇰")
     (image . "flag_ck.png")
     (style . "github"))
    (:flag_ch:
     (unicode . "🇨🇭")
     (image . "flag_ch.png")
     (style . "github"))
    (:flag-vn:
     (unicode . "🇻🇳")
     (image . "flag_vn.png")
     (style . "github"))
    (:flag-iq:
     (unicode . "🇮🇶")
     (image . "flag_iq.png")
     (style . "github"))
    (:lc:
     (unicode . "🇱🇨")
     (image . "flag_lc.png")
     (style . "github"))
    (:snow_capped_mountain:
     (unicode . "🏔")
     (image . "mountain_snow.png")
     (style . "github"))
    (:wolf:
     (unicode . "🐺")
     (image . "wolf.png")
     (style . "github"))
    (:ly:
     (unicode . "🇱🇾")
     (image . "flag_ly.png")
     (style . "github"))
    (:princess_tone4:
     (unicode . "👸🏾")
     (image . "princess_tone4.png")
     (style . "github"))
    (:moyai:
     (unicode . "🗿")
     (image . "moyai.png")
     (style . "github"))
    (:arrow_down:
     (unicode . "⬇")
     (image . "arrow_down.png")
     (style . "github"))
    (:santa_tone5:
     (unicode . "🎅🏿")
     (image . "santa_tone5.png")
     (style . "github"))
    (:mountain_cableway:
     (unicode . "🚠")
     (image . "mountain_cableway.png")
     (style . "github"))
    (:horse_racing_tone4:
     (unicode . "🏇🏾")
     (image . "horse_racing_tone4.png")
     (style . "github"))
    (:arrow_up_down:
     (unicode . "↕")
     (image . "arrow_up_down.png")
     (style . "github"))
    (:grinning:
     (unicode . "😀")
     (image . "grinning.png")
     (style . "github"))
    (:massage-tone3:
     (unicode . "💆🏽")
     (image . "massage_tone3.png")
     (style . "github"))
    (:card_box:
     (unicode . "🗃")
     (image . "card_box.png")
     (style . "github"))
    (:do-not-litter:
     (unicode . "🚯")
     (image . "do_not_litter.png")
     (style . "github"))
    (:flag_im:
     (unicode . "🇮🇲")
     (image . "flag_im.png")
     (style . "github"))
    (:sparkler:
     (unicode . "🎇")
     (image . "sparkler.png")
     (style . "github"))
    (:boy_tone5:
     (unicode . "👦🏿")
     (image . "boy_tone5.png")
     (style . "github"))
    (:wavy-dash:
     (unicode . "〰")
     (image . "wavy_dash.png")
     (style . "github"))
    (:water_buffalo:
     (unicode . "🐃")
     (image . "water_buffalo.png")
     (style . "github"))
    (:diamonds:
     (unicode . "♦")
     (image . "diamonds.png")
     (style . "github"))
    (:tone5:
     (unicode . "🏿")
     (image . "tone5.png")
     (style . "github"))
    (:flag_cz:
     (unicode . "🇨🇿")
     (image . "flag_cz.png")
     (style . "github"))
    (:nose:
     (unicode . "👃")
     (image . "nose.png")
     (style . "github"))
    (:ear-tone1:
     (unicode . "👂🏻")
     (image . "ear_tone1.png")
     (style . "github"))
    (:rice-cracker:
     (unicode . "🍘")
     (image . "rice_cracker.png")
     (style . "github"))
    (:pray_tone1:
     (unicode . "🙏🏻")
     (image . "pray_tone1.png")
     (style . "github"))
    (:musical_score:
     (unicode . "🎼")
     (image . "musical_score.png")
     (style . "github"))
    (:family_mwgg:
     (unicode . "👨👩👧👧")
     (image . "family_mwgg.png")
     (style . "github"))
    (:ok-woman-tone4:
     (unicode . "🙆🏾")
     (image . "ok_woman_tone4.png")
     (style . "github"))
    (:computer:
     (unicode . "💻")
     (image . "computer.png")
     (style . "github"))
    (:flag_ar:
     (unicode . "🇦🇷")
     (image . "flag_ar.png")
     (style . "github"))
    (:point_right_tone3:
     (unicode . "👉🏽")
     (image . "point_right_tone3.png")
     (style . "github"))
    (:wave_tone2:
     (unicode . "👋🏼")
     (image . "wave_tone2.png")
     (style . "github"))
    (:sign_of_the_horns_tone3:
     (unicode . "🤘🏽")
     (image . "metal_tone3.png")
     (style . "github"))
    (:wave_tone1:
     (unicode . "👋🏻")
     (image . "wave_tone1.png")
     (style . "github"))
    (:video-game:
     (unicode . "🎮")
     (image . "video_game.png")
     (style . "github"))
    (:honey-pot:
     (unicode . "🍯")
     (image . "honey_pot.png")
     (style . "github"))
    (:tomato:
     (unicode . "🍅")
     (image . "tomato.png")
     (style . "github"))
    (:flag_au:
     (unicode . "🇦🇺")
     (image . "flag_au.png")
     (style . "github"))
    (:no_mobile_phones:
     (unicode . "📵")
     (image . "no_mobile_phones.png")
     (style . "github"))
    (:flag_bi:
     (unicode . "🇧🇮")
     (image . "flag_bi.png")
     (style . "github"))
    (:aerial-tramway:
     (unicode . "🚡")
     (image . "aerial_tramway.png")
     (style . "github"))
    (:flag_pn:
     (unicode . "🇵🇳")
     (image . "flag_pn.png")
     (style . "github"))
    (:interrobang:
     (unicode . "⁉")
     (image . "interrobang.png")
     (style . "github"))
    (:prayer-beads:
     (unicode . "📿")
     (image . "prayer_beads.png")
     (style . "github"))
    (:race-car:
     (unicode . "🏎")
     (image . "race_car.png")
     (style . "github"))
    (:mf:
     (unicode . "🇲🇫")
     (image . "flag_mf.png")
     (style . "github"))
    (:flag_vn:
     (unicode . "🇻🇳")
     (image . "flag_vn.png")
     (style . "github"))
    (:sunny:
     (unicode . "☀")
     (image . "sunny.png")
     (style . "github"))
    (:thumbsdown-tone1:
     (unicode . "👎🏻")
     (image . "thumbsdown_tone1.png")
     (style . "github"))
    (:point_right:
     (unicode . "👉")
     (image . "point_right.png")
     (style . "github"))
    (:spiral_note_pad:
     (unicode . "🗒")
     (image . "notepad_spiral.png")
     (style . "github"))
    (:no-good-tone2:
     (unicode . "🙅🏼")
     (image . "no_good_tone2.png")
     (style . "github"))
    (:flag-pk:
     (unicode . "🇵🇰")
     (image . "flag_pk.png")
     (style . "github"))
    (:cloud:
     (unicode . "☁")
     (image . "cloud.png")
     (style . "github"))
    (:small_blue_diamond:
     (unicode . "🔹")
     (image . "small_blue_diamond.png")
     (style . "github"))
    (:nigeria:
     (unicode . "🇳🇬")
     (image . "flag_ng.png")
     (style . "github"))
    (:td:
     (unicode . "🇹🇩")
     (image . "flag_td.png")
     (style . "github"))
    (:couple_with_heart:
     (unicode . "💑")
     (image . "couple_with_heart.png")
     (style . "github"))
    (:arrow-down:
     (unicode . "⬇")
     (image . "arrow_down.png")
     (style . "github"))
    (:ec:
     (unicode . "🇪🇨")
     (image . "flag_ec.png")
     (style . "github"))
    (:black_small_square:
     (unicode . "▪")
     (image . "black_small_square.png")
     (style . "github"))
    (:baby-symbol:
     (unicode . "🚼")
     (image . "baby_symbol.png")
     (style . "github"))
    (:flag_la:
     (unicode . "🇱🇦")
     (image . "flag_la.png")
     (style . "github"))
    (:mountain-bicyclist-tone5:
     (unicode . "🚵🏿")
     (image . "mountain_bicyclist_tone5.png")
     (style . "github"))
    (:flag_sz:
     (unicode . "🇸🇿")
     (image . "flag_sz.png")
     (style . "github"))
    (:stuck-out-tongue:
     (ascii . ":P")
     (unicode . "😛")
     (image . "stuck_out_tongue.png")
     (style . "github"))
    (:weary:
     (unicode . "😩")
     (image . "weary.png")
     (style . "github"))
    (:cop_tone1:
     (unicode . "👮🏻")
     (image . "cop_tone1.png")
     (style . "github"))
    (:relieved:
     (unicode . "😌")
     (image . "relieved.png")
     (style . "github"))
    (:sunrise_over_mountains:
     (unicode . "🌄")
     (image . "sunrise_over_mountains.png")
     (style . "github"))
    (:flag-as:
     (unicode . "🇦🇸")
     (image . "flag_as.png")
     (style . "github"))
    (:lower_left_paintbrush:
     (unicode . "🖌")
     (image . "paintbrush.png")
     (style . "github"))
    (:flag_ic:
     (unicode . "🇮🇨")
     (image . "flag_ic.png")
     (style . "github"))
    (:tongue:
     (unicode . "👅")
     (image . "tongue.png")
     (style . "github"))
    (:tv:
     (unicode . "📺")
     (image . "tv.png")
     (style . "github"))
    (:mount-fuji:
     (unicode . "🗻")
     (image . "mount_fuji.png")
     (style . "github"))
    (:briefcase:
     (unicode . "💼")
     (image . "briefcase.png")
     (style . "github"))
    (:flag_me:
     (unicode . "🇲🇪")
     (image . "flag_me.png")
     (style . "github"))
    (:coffee:
     (unicode . "☕")
     (image . "coffee.png")
     (style . "github"))
    (:thumbsdown_tone3:
     (unicode . "👎🏽")
     (image . "thumbsdown_tone3.png")
     (style . "github"))
    (:flag-np:
     (unicode . "🇳🇵")
     (image . "flag_np.png")
     (style . "github"))
    (:articulated_lorry:
     (unicode . "🚛")
     (image . "articulated_lorry.png")
     (style . "github"))
    (:crocodile:
     (unicode . "🐊")
     (image . "crocodile.png")
     (style . "github"))
    (:flag_uz:
     (unicode . "🇺🇿")
     (image . "flag_uz.png")
     (style . "github"))
    (:older-man-tone1:
     (unicode . "👴🏻")
     (image . "older_man_tone1.png")
     (style . "github"))
    (:ear_of_rice:
     (unicode . "🌾")
     (image . "ear_of_rice.png")
     (style . "github"))
    (:raising-hand-tone4:
     (unicode . "🙋🏾")
     (image . "raising_hand_tone4.png")
     (style . "github"))
    (:white_sun_with_small_cloud:
     (unicode . "🌤")
     (image . "white_sun_small_cloud.png")
     (style . "github"))
    (:man_with_gua_pi_mao:
     (unicode . "👲")
     (image . "man_with_gua_pi_mao.png")
     (style . "github"))
    (:mountain-railway:
     (unicode . "🚞")
     (image . "mountain_railway.png")
     (style . "github"))
    (:flag_ke:
     (unicode . "🇰🇪")
     (image . "flag_ke.png")
     (style . "github"))
    (:red-car:
     (unicode . "🚗")
     (image . "red_car.png")
     (style . "github"))
    (:first_quarter_moon:
     (unicode . "🌓")
     (image . "first_quarter_moon.png")
     (style . "github"))
    (:love-letter:
     (unicode . "💌")
     (image . "love_letter.png")
     (style . "github"))
    (:revolving_hearts:
     (unicode . "💞")
     (image . "revolving_hearts.png")
     (style . "github"))
    (:flag_mv:
     (unicode . "🇲🇻")
     (image . "flag_mv.png")
     (style . "github"))
    (:speedboat:
     (unicode . "🚤")
     (image . "speedboat.png")
     (style . "github"))
    (:flag_iq:
     (unicode . "🇮🇶")
     (image . "flag_iq.png")
     (style . "github"))
    (:whale:
     (unicode . "🐳")
     (image . "whale.png")
     (style . "github"))
    (:km:
     (unicode . "🇰🇲")
     (image . "flag_km.png")
     (style . "github"))
    (:waxing_gibbous_moon:
     (unicode . "🌔")
     (image . "waxing_gibbous_moon.png")
     (style . "github"))
    (:flag-re:
     (unicode . "🇷🇪")
     (image . "flag_re.png")
     (style . "github"))
    (:no:
     (unicode . "🇳🇴")
     (image . "flag_no.png")
     (style . "github"))
    (:passenger_ship:
     (unicode . "🛳")
     (image . "cruise_ship.png")
     (style . "github"))
    (:wave-tone2:
     (unicode . "👋🏼")
     (image . "wave_tone2.png")
     (style . "github"))
    (:clock9:
     (unicode . "🕘")
     (image . "clock9.png")
     (style . "github"))
    (:person-with-blond-hair-tone1:
     (unicode . "👱🏻")
     (image . "person_with_blond_hair_tone1.png")
     (style . "github"))
    (:flag_hk:
     (unicode . "🇭🇰")
     (image . "flag_hk.png")
     (style . "github"))
    (:angel-tone4:
     (unicode . "👼🏾")
     (image . "angel_tone4.png")
     (style . "github"))
    (:hamster:
     (unicode . "🐹")
     (image . "hamster.png")
     (style . "github"))
    (:hearts:
     (unicode . "♥")
     (image . "hearts.png")
     (style . "github"))
    (:sweat-smile:
     (ascii . "':)")
     (unicode . "😅")
     (image . "sweat_smile.png")
     (style . "github"))
    (:flag-br:
     (unicode . "🇧🇷")
     (image . "flag_br.png")
     (style . "github"))
    (:couple_with_heart_ww:
     (unicode . "👩❤👩")
     (image . "couple_ww.png")
     (style . "github"))
    (:rice_scene:
     (unicode . "🎑")
     (image . "rice_scene.png")
     (style . "github"))
    (:fuelpump:
     (unicode . "⛽")
     (image . "fuelpump.png")
     (style . "github"))
    (:alembic:
     (unicode . "⚗")
     (image . "alembic.png")
     (style . "github"))
    (:flag_nc:
     (unicode . "🇳🇨")
     (image . "flag_nc.png")
     (style . "github"))
    (:grandma_tone4:
     (unicode . "👵🏾")
     (image . "older_woman_tone4.png")
     (style . "github"))
    (:flag-dj:
     (unicode . "🇩🇯")
     (image . "flag_dj.png")
     (style . "github"))
    (:slot_machine:
     (unicode . "🎰")
     (image . "slot_machine.png")
     (style . "github"))
    (:cricket_bat_ball:
     (unicode . "🏏")
     (image . "cricket.png")
     (style . "github"))
    (:flag-jp:
     (unicode . "🇯🇵")
     (image . "flag_jp.png")
     (style . "github"))
    (:v_tone4:
     (unicode . "✌🏾")
     (image . "v_tone4.png")
     (style . "github"))
    (:construction-worker-tone3:
     (unicode . "👷🏽")
     (image . "construction_worker_tone3.png")
     (style . "github"))
    (:flag-ad:
     (unicode . "🇦🇩")
     (image . "flag_ad.png")
     (style . "github"))
    (:watch:
     (unicode . "⌚")
     (image . "watch.png")
     (style . "github"))
    (:flag_cx:
     (unicode . "🇨🇽")
     (image . "flag_cx.png")
     (style . "github"))
    (:jack-o-lantern:
     (unicode . "🎃")
     (image . "jack_o_lantern.png")
     (style . "github"))
    (:flag_st:
     (unicode . "🇸🇹")
     (image . "flag_st.png")
     (style . "github"))
    (:flag-fj:
     (unicode . "🇫🇯")
     (image . "flag_fj.png")
     (style . "github"))
    (:camera-with-flash:
     (unicode . "📸")
     (image . "camera_with_flash.png")
     (style . "github"))
    (:flag_gu:
     (unicode . "🇬🇺")
     (image . "flag_gu.png")
     (style . "github"))
    (:clap-tone5:
     (unicode . "👏🏿")
     (image . "clap_tone5.png")
     (style . "github"))
    (:-\(
     (unicode . "😞")
     (image . "disappointed.png")
     (style . "ascii"))
    (:ls:
     (unicode . "🇱🇸")
     (image . "flag_ls.png")
     (style . "github"))
    (:rowboat-tone1:
     (unicode . "🚣🏻")
     (image . "rowboat_tone1.png")
     (style . "github"))
    (:bo:
     (unicode . "🇧🇴")
     (image . "flag_bo.png")
     (style . "github"))
    (:person_frowning_tone1:
     (unicode . "🙍🏻")
     (image . "person_frowning_tone1.png")
     (style . "github"))
    (:flag-kw:
     (unicode . "🇰🇼")
     (image . "flag_kw.png")
     (style . "github"))
    (:haircut-tone2:
     (unicode . "💇🏼")
     (image . "haircut_tone2.png")
     (style . "github"))
    (:weight_lifter_tone5:
     (unicode . "🏋🏿")
     (image . "lifter_tone5.png")
     (style . "github"))
    (:flag_gg:
     (unicode . "🇬🇬")
     (image . "flag_gg.png")
     (style . "github"))
    (:information_desk_person_tone5:
     (unicode . "💁🏿")
     (image . "information_desk_person_tone5.png")
     (style . "github"))
    (:crossed_flags:
     (unicode . "🎌")
     (image . "crossed_flags.png")
     (style . "github"))
    (:kissing_closed_eyes:
     (unicode . "😚")
     (image . "kissing_closed_eyes.png")
     (style . "github"))
    (:flag_eg:
     (unicode . "🇪🇬")
     (image . "flag_eg.png")
     (style . "github"))
    (:dancer-tone3:
     (unicode . "💃🏽")
     (image . "dancer_tone3.png")
     (style . "github"))
    (:cloud-lightning:
     (unicode . "🌩")
     (image . "cloud_lightning.png")
     (style . "github"))
    (:full_moon_with_face:
     (unicode . "🌝")
     (image . "full_moon_with_face.png")
     (style . "github"))
    (:six_pointed_star:
     (unicode . "🔯")
     (image . "six_pointed_star.png")
     (style . "github"))
    (:orthodox_cross:
     (unicode . "☦")
     (image . "orthodox_cross.png")
     (style . "github"))
    (:writing_hand_tone4:
     (unicode . "✍🏾")
     (image . "writing_hand_tone4.png")
     (style . "github"))
    (:jeans:
     (unicode . "👖")
     (image . "jeans.png")
     (style . "github"))
    (:flag-ee:
     (unicode . "🇪🇪")
     (image . "flag_ee.png")
     (style . "github"))
    (:man-with-turban:
     (unicode . "👳")
     (image . "man_with_turban.png")
     (style . "github"))
    (:ok_woman_tone4:
     (unicode . "🙆🏾")
     (image . "ok_woman_tone4.png")
     (style . "github"))
    (:mute:
     (unicode . "🔇")
     (image . "mute.png")
     (style . "github"))
    (:small-red-triangle:
     (unicode . "🔺")
     (image . "small_red_triangle.png")
     (style . "github"))
    (:flag-gd:
     (unicode . "🇬🇩")
     (image . "flag_gd.png")
     (style . "github"))
    (:ear-tone3:
     (unicode . "👂🏽")
     (image . "ear_tone3.png")
     (style . "github"))
    (:classical-building:
     (unicode . "🏛")
     (image . "classical_building.png")
     (style . "github"))
    (:gi:
     (unicode . "🇬🇮")
     (image . "flag_gi.png")
     (style . "github"))
    (:nose-tone2:
     (unicode . "👃🏼")
     (image . "nose_tone2.png")
     (style . "github"))
    (:point-right-tone1:
     (unicode . "👉🏻")
     (image . "point_right_tone1.png")
     (style . "github"))
    (:flag-hr:
     (unicode . "🇭🇷")
     (image . "flag_hr.png")
     (style . "github"))
    (:city_sunrise:
     (unicode . "🌇")
     (image . "city_sunset.png")
     (style . "github"))
    (:biohazard_sign:
     (unicode . "☣")
     (image . "biohazard.png")
     (style . "github"))
    (:kissing_smiling_eyes:
     (unicode . "😙")
     (image . "kissing_smiling_eyes.png")
     (style . "github"))
    (:im:
     (unicode . "🇮🇲")
     (image . "flag_im.png")
     (style . "github"))
    (:flag-sg:
     (unicode . "🇸🇬")
     (image . "flag_sg.png")
     (style . "github"))
    (:family-mmb:
     (unicode . "👨👨👦")
     (image . "family_mmb.png")
     (style . "github"))
    (:school-satchel:
     (unicode . "🎒")
     (image . "school_satchel.png")
     (style . "github"))
    (:raising_hand_tone4:
     (unicode . "🙋🏾")
     (image . "raising_hand_tone4.png")
     (style . "github"))
    (:haircut_tone2:
     (unicode . "💇🏼")
     (image . "haircut_tone2.png")
     (style . "github"))
    (:person-with-blond-hair:
     (unicode . "👱")
     (image . "person_with_blond_hair.png")
     (style . "github"))
    (:heart_exclamation:
     (unicode . "❣")
     (image . "heart_exclamation.png")
     (style . "github"))
    (:scream_cat:
     (unicode . "🙀")
     (image . "scream_cat.png")
     (style . "github"))
    (:flag-kn:
     (unicode . "🇰🇳")
     (image . "flag_kn.png")
     (style . "github"))
    (:flag-bd:
     (unicode . "🇧🇩")
     (image . "flag_bd.png")
     (style . "github"))
    (:no-good-tone4:
     (unicode . "🙅🏾")
     (image . "no_good_tone4.png")
     (style . "github"))
    (:flag_jo:
     (unicode . "🇯🇴")
     (image . "flag_jo.png")
     (style . "github"))
    (:flag-mp:
     (unicode . "🇲🇵")
     (image . "flag_mp.png")
     (style . "github"))
    (:place_of_worship:
     (unicode . "🛐")
     (image . "place_of_worship.png")
     (style . "github"))
    (:person-frowning:
     (unicode . "🙍")
     (image . "person_frowning.png")
     (style . "github"))
    (:flag-mx:
     (unicode . "🇲🇽")
     (image . "flag_mx.png")
     (style . "github"))
    (:flag_by:
     (unicode . "🇧🇾")
     (image . "flag_by.png")
     (style . "github"))
    (:flag-uy:
     (unicode . "🇺🇾")
     (image . "flag_uy.png")
     (style . "github"))
    (:ai:
     (unicode . "🇦🇮")
     (image . "flag_ai.png")
     (style . "github"))
    (:flag_tc:
     (unicode . "🇹🇨")
     (image . "flag_tc.png")
     (style . "github"))
    (:ticket:
     (unicode . "🎫")
     (image . "ticket.png")
     (style . "github"))
    (:thermometer_face:
     (unicode . "🤒")
     (image . "thermometer_face.png")
     (style . "github"))
    (:person_with_pouting_face_tone1:
     (unicode . "🙎🏻")
     (image . "person_with_pouting_face_tone1.png")
     (style . "github"))
    (:circus_tent:
     (unicode . "🎪")
     (image . "circus_tent.png")
     (style . "github"))
    (:clap_tone3:
     (unicode . "👏🏽")
     (image . "clap_tone3.png")
     (style . "github"))
    (:swimmer:
     (unicode . "🏊")
     (image . "swimmer.png")
     (style . "github"))
    (:blue-car:
     (unicode . "🚙")
     (image . "blue_car.png")
     (style . "github"))
    (:two_men_holding_hands:
     (unicode . "👬")
     (image . "two_men_holding_hands.png")
     (style . "github"))
    (:cloud-tornado:
     (unicode . "🌪")
     (image . "cloud_tornado.png")
     (style . "github"))
    (:rooster:
     (unicode . "🐓")
     (image . "rooster.png")
     (style . "github"))
    (:flag_as:
     (unicode . "🇦🇸")
     (image . "flag_as.png")
     (style . "github"))
    (:world_map:
     (unicode . "🗺")
     (image . "map.png")
     (style . "github"))
    (:arrow_left:
     (unicode . "⬅")
     (image . "arrow_left.png")
     (style . "github"))
    (:ad:
     (unicode . "🇦🇩")
     (image . "flag_ad.png")
     (style . "github"))
    (:satellite:
     (unicode . "📡")
     (image . "satellite.png")
     (style . "github"))
    (:thumbsdown-tone3:
     (unicode . "👎🏽")
     (image . "thumbsdown_tone3.png")
     (style . "github"))
    (:arrow-upper-left:
     (unicode . "↖")
     (image . "arrow_upper_left.png")
     (style . "github"))
    (:santa-tone2:
     (unicode . "🎅🏼")
     (image . "santa_tone2.png")
     (style . "github"))
    (:flag-ro:
     (unicode . "🇷🇴")
     (image . "flag_ro.png")
     (style . "github"))
    (:customs:
     (unicode . "🛃")
     (image . "customs.png")
     (style . "github"))
    (:flag-na:
     (unicode . "🇳🇦")
     (image . "flag_na.png")
     (style . "github"))
    (:sign_of_the_horns_tone2:
     (unicode . "🤘🏼")
     (image . "metal_tone2.png")
     (style . "github"))
    (:older-woman-tone2:
     (unicode . "👵🏼")
     (image . "older_woman_tone2.png")
     (style . "github"))
    (:fist-tone1:
     (unicode . "✊🏻")
     (image . "fist_tone1.png")
     (style . "github"))
    (:baby-tone1:
     (unicode . "👶🏻")
     (image . "baby_tone1.png")
     (style . "github"))
    (:flag-to:
     (unicode . "🇹🇴")
     (image . "flag_to.png")
     (style . "github"))
    (:vi:
     (unicode . "🇻🇮")
     (image . "flag_vi.png")
     (style . "github"))
    (:arrow_up:
     (unicode . "⬆")
     (image . "arrow_up.png")
     (style . "github"))
    (:sign_of_the_horns_tone5:
     (unicode . "🤘🏿")
     (image . "metal_tone5.png")
     (style . "github"))
    (:older_man_tone4:
     (unicode . "👴🏾")
     (image . "older_man_tone4.png")
     (style . "github"))
    (:u7981:
     (unicode . "🈲")
     (image . "u7981.png")
     (style . "github"))
    (:lb:
     (unicode . "🇱🇧")
     (image . "flag_lb.png")
     (style . "github"))
    (:flag_ck:
     (unicode . "🇨🇰")
     (image . "flag_ck.png")
     (style . "github"))
    (:flag_do:
     (unicode . "🇩🇴")
     (image . "flag_do.png")
     (style . "github"))
    (:mask:
     (unicode . "😷")
     (image . "mask.png")
     (style . "github"))
    (:arrow_upper_right:
     (unicode . "↗")
     (image . "arrow_upper_right.png")
     (style . "github"))
    (:clock330:
     (unicode . "🕞")
     (image . "clock330.png")
     (style . "github"))
    (:flag_ac:
     (unicode . "🇦🇨")
     (image . "flag_ac.png")
     (style . "github"))
    (:up:
     (unicode . "🆙")
     (image . "up.png")
     (style . "github"))
    (:santa-tone1:
     (unicode . "🎅🏻")
     (image . "santa_tone1.png")
     (style . "github"))
    (:cm:
     (unicode . "🇨🇲")
     (image . "flag_cm.png")
     (style . "github"))
    (:massage-tone2:
     (unicode . "💆🏼")
     (image . "massage_tone2.png")
     (style . "github"))
    (:credit-card:
     (unicode . "💳")
     (image . "credit_card.png")
     (style . "github"))
    (:vg:
     (unicode . "🇻🇬")
     (image . "flag_vg.png")
     (style . "github"))
    (:new_moon_with_face:
     (unicode . "🌚")
     (image . "new_moon_with_face.png")
     (style . "github"))
    (:cloud_lightning:
     (unicode . "🌩")
     (image . "cloud_lightning.png")
     (style . "github"))
    (:flag_aq:
     (unicode . "🇦🇶")
     (image . "flag_aq.png")
     (style . "github"))
    (:tone4:
     (unicode . "🏾")
     (image . "tone4.png")
     (style . "github"))
    (:boy_tone2:
     (unicode . "👦🏼")
     (image . "boy_tone2.png")
     (style . "github"))
    (:raised_hand_with_part_between_middle_and_ring_fingers:
     (unicode . "🖖")
     (image . "vulcan.png")
     (style . "github"))
    (:fish:
     (unicode . "🐟")
     (image . "fish.png")
     (style . "github"))
    (:dog:
     (unicode . "🐶")
     (image . "dog.png")
     (style . "github"))
    (:racehorse:
     (unicode . "🐎")
     (image . "racehorse.png")
     (style . "github"))
    (:sa:
     (unicode . "🈂")
     (image . "sa.png")
     (style . "github"))
    (:shaved_ice:
     (unicode . "🍧")
     (image . "shaved_ice.png")
     (style . "github"))
    (:point_right_tone2:
     (unicode . "👉🏼")
     (image . "point_right_tone2.png")
     (style . "github"))
    (:corn:
     (unicode . "🌽")
     (image . "corn.png")
     (style . "github"))
    (:um:
     (unicode . "🇺🇲")
     (image . "flag_um.png")
     (style . "github"))
    (:flag-cz:
     (unicode . "🇨🇿")
     (image . "flag_cz.png")
     (style . "github"))
    (:person-frowning-tone4:
     (unicode . "🙍🏾")
     (image . "person_frowning_tone4.png")
     (style . "github"))
    (:flag_va:
     (unicode . "🇻🇦")
     (image . "flag_va.png")
     (style . "github"))
    (:cop-tone2:
     (unicode . "👮🏼")
     (image . "cop_tone2.png")
     (style . "github"))
    (:flag_mz:
     (unicode . "🇲🇿")
     (image . "flag_mz.png")
     (style . "github"))
    (:flag_white:
     (unicode . "🏳")
     (image . "flag_white.png")
     (style . "github"))
    (:milky_way:
     (unicode . "🌌")
     (image . "milky_way.png")
     (style . "github"))
    (:nail_care_tone4:
     (unicode . "💅🏾")
     (image . "nail_care_tone4.png")
     (style . "github"))
    (:floppy_disk:
     (unicode . "💾")
     (image . "floppy_disk.png")
     (style . "github"))
    (:guardsman-tone2:
     (unicode . "💂🏼")
     (image . "guardsman_tone2.png")
     (style . "github"))
    (:swimmer_tone2:
     (unicode . "🏊🏼")
     (image . "swimmer_tone2.png")
     (style . "github"))
    (:cop-tone3:
     (unicode . "👮🏽")
     (image . "cop_tone3.png")
     (style . "github"))
    (:runner-tone2:
     (unicode . "🏃🏼")
     (image . "runner_tone2.png")
     (style . "github"))
    (:blue-heart:
     (unicode . "💙")
     (image . "blue_heart.png")
     (style . "github"))
    (:arrow_up_small:
     (unicode . "🔼")
     (image . "arrow_up_small.png")
     (style . "github"))
    (:rw:
     (unicode . "🇷🇼")
     (image . "flag_rw.png")
     (style . "github"))
    (:hand_splayed:
     (unicode . "🖐")
     (image . "hand_splayed.png")
     (style . "github"))
    (:raised_hand_with_fingers_splayed_tone5:
     (unicode . "🖐🏿")
     (image . "hand_splayed_tone5.png")
     (style . "github"))
    (:ok-woman:
     (ascii . "*\\0/*")
     (unicode . "🙆")
     (image . "ok_woman.png")
     (style . "github"))
    (:satellite-orbital:
     (unicode . "🛰")
     (image . "satellite_orbital.png")
     (style . "github"))
    (:lifter_tone1:
     (unicode . "🏋🏻")
     (image . "lifter_tone1.png")
     (style . "github"))
    (:sleeping:
     (unicode . "😴")
     (image . "sleeping.png")
     (style . "github"))
    (:flag_kz:
     (unicode . "🇰🇿")
     (image . "flag_kz.png")
     (style . "github"))
    (:ht:
     (unicode . "🇭🇹")
     (image . "flag_ht.png")
     (style . "github"))
    (:spider_web:
     (unicode . "🕸")
     (image . "spider_web.png")
     (style . "github"))
    (:cop:
     (unicode . "👮")
     (image . "cop.png")
     (style . "github"))
    (:crayon:
     (unicode . "🖍")
     (image . "crayon.png")
     (style . "github"))
    (:tg:
     (unicode . "🇹🇬")
     (image . "flag_tg.png")
     (style . "github"))
    (:flag-no:
     (unicode . "🇳🇴")
     (image . "flag_no.png")
     (style . "github"))
    (:point-down-tone5:
     (unicode . "👇🏿")
     (image . "point_down_tone5.png")
     (style . "github"))
    (:flag_ae:
     (unicode . "🇦🇪")
     (image . "flag_ae.png")
     (style . "github"))
    (:signal-strength:
     (unicode . "📶")
     (image . "signal_strength.png")
     (style . "github"))
    (:red-circle:
     (unicode . "🔴")
     (image . "red_circle.png")
     (style . "github"))
    (:on:
     (unicode . "🔛")
     (image . "on.png")
     (style . "github"))
    (:rice:
     (unicode . "🍚")
     (image . "rice.png")
     (style . "github"))
    (:mountain-bicyclist-tone2:
     (unicode . "🚵🏼")
     (image . "mountain_bicyclist_tone2.png")
     (style . "github"))
    (:wf:
     (unicode . "🇼🇫")
     (image . "flag_wf.png")
     (style . "github"))
    (:re:
     (unicode . "🇷🇪")
     (image . "flag_re.png")
     (style . "github"))
    (:pray-tone2:
     (unicode . "🙏🏼")
     (image . "pray_tone2.png")
     (style . "github"))
    (:flag-rs:
     (unicode . "🇷🇸")
     (image . "flag_rs.png")
     (style . "github"))
    (:earth-africa:
     (unicode . "🌍")
     (image . "earth_africa.png")
     (style . "github"))
    (:baby_symbol:
     (unicode . "🚼")
     (image . "baby_symbol.png")
     (style . "github"))
    (:pig:
     (unicode . "🐷")
     (image . "pig.png")
     (style . "github"))
    (:person_with_ball_tone5:
     (unicode . "⛹🏿")
     (image . "basketball_player_tone5.png")
     (style . "github"))
    (:control_knobs:
     (unicode . "🎛")
     (image . "control_knobs.png")
     (style . "github"))
    (:city_sunset:
     (unicode . "🌇")
     (image . "city_sunset.png")
     (style . "github"))
    (:bookmark-tabs:
     (unicode . "📑")
     (image . "bookmark_tabs.png")
     (style . "github"))
    (:girl-tone4:
     (unicode . "👧🏾")
     (image . "girl_tone4.png")
     (style . "github"))
    (:mouse_three_button:
     (unicode . "🖱")
     (image . "mouse_three_button.png")
     (style . "github"))
    (:arrow-down-small:
     (unicode . "🔽")
     (image . "arrow_down_small.png")
     (style . "github"))
    (:baby_tone4:
     (unicode . "👶🏾")
     (image . "baby_tone4.png")
     (style . "github"))
    (:vu:
     (unicode . "🇻🇺")
     (image . "flag_vu.png")
     (style . "github"))
    (:person_frowning:
     (unicode . "🙍")
     (image . "person_frowning.png")
     (style . "github"))
    (:flag_ro:
     (unicode . "🇷🇴")
     (image . "flag_ro.png")
     (style . "github"))
    (:flag_cw:
     (unicode . "🇨🇼")
     (image . "flag_cw.png")
     (style . "github"))
    (:flag_uy:
     (unicode . "🇺🇾")
     (image . "flag_uy.png")
     (style . "github"))
    (:flag-sh:
     (unicode . "🇸🇭")
     (image . "flag_sh.png")
     (style . "github"))
    (:military-medal:
     (unicode . "🎖")
     (image . "military_medal.png")
     (style . "github"))
    (:european-castle:
     (unicode . "🏰")
     (image . "european_castle.png")
     (style . "github"))
    (:spider:
     (unicode . "🕷")
     (image . "spider.png")
     (style . "github"))
    (:taco:
     (unicode . "🌮")
     (image . "taco.png")
     (style . "github"))
    (:ballot-box:
     (unicode . "🗳")
     (image . "ballot_box.png")
     (style . "github"))
    (:umbrella_on_ground:
     (unicode . "⛱")
     (image . "beach_umbrella.png")
     (style . "github"))
    (:heavy-check-mark:
     (unicode . "✔")
     (image . "heavy_check_mark.png")
     (style . "github"))
    (:email:
     (unicode . "📧")
     (image . "e_mail.png")
     (style . "github"))
    (:speech-balloon:
     (unicode . "💬")
     (image . "speech_balloon.png")
     (style . "github"))
    (:saudi:
     (unicode . "🇸🇦")
     (image . "flag_sa.png")
     (style . "github"))
    (:wine_glass:
     (unicode . "🍷")
     (image . "wine_glass.png")
     (style . "github"))
    (:punch:
     (unicode . "👊")
     (image . "punch.png")
     (style . "github"))
    (:flag-nr:
     (unicode . "🇳🇷")
     (image . "flag_nr.png")
     (style . "github"))
    (:office:
     (unicode . "🏢")
     (image . "office.png")
     (style . "github"))
    (:notepad_spiral:
     (unicode . "🗒")
     (image . "notepad_spiral.png")
     (style . "github"))
    (:-D
     (unicode . "😃")
     (image . "smiley.png")
     (style . "ascii"))
    (:bug:
     (unicode . "🐛")
     (image . "bug.png")
     (style . "github"))
    (:eye:
     (unicode . "👁")
     (image . "eye.png")
     (style . "github"))
    (:heavy_division_sign:
     (unicode . "➗")
     (image . "heavy_division_sign.png")
     (style . "github"))
    (:wave-tone5:
     (unicode . "👋🏿")
     (image . "wave_tone5.png")
     (style . "github"))
    (:maple_leaf:
     (unicode . "🍁")
     (image . "maple_leaf.png")
     (style . "github"))
    (:bath:
     (unicode . "🛀")
     (image . "bath.png")
     (style . "github"))
    (:family-mwbb:
     (unicode . "👨👩👦👦")
     (image . "family_mwbb.png")
     (style . "github"))
    (:u6e80:
     (unicode . "🈵")
     (image . "u6e80.png")
     (style . "github"))
    (:bath_tone5:
     (unicode . "🛀🏿")
     (image . "bath_tone5.png")
     (style . "github"))
    (:flag-ag:
     (unicode . "🇦🇬")
     (image . "flag_ag.png")
     (style . "github"))
    (:robot:
     (unicode . "🤖")
     (image . "robot.png")
     (style . "github"))
    (:flag-bs:
     (unicode . "🇧🇸")
     (image . "flag_bs.png")
     (style . "github"))
    (:city-dusk:
     (unicode . "🌆")
     (image . "city_dusk.png")
     (style . "github"))
    (:thumbsup-tone1:
     (unicode . "👍🏻")
     (image . "thumbsup_tone1.png")
     (style . "github"))
    (:couch:
     (unicode . "🛋")
     (image . "couch.png")
     (style . "github"))
    (:older_man_tone2:
     (unicode . "👴🏼")
     (image . "older_man_tone2.png")
     (style . "github"))
    (:arrow-up:
     (unicode . "⬆")
     (image . "arrow_up.png")
     (style . "github"))
    (:bride-with-veil-tone5:
     (unicode . "👰🏿")
     (image . "bride_with_veil_tone5.png")
     (style . "github"))
    (:flag-dk:
     (unicode . "🇩🇰")
     (image . "flag_dk.png")
     (style . "github"))
    (:beers:
     (unicode . "🍻")
     (image . "beers.png")
     (style . "github"))
    (:grandma_tone5:
     (unicode . "👵🏿")
     (image . "older_woman_tone5.png")
     (style . "github"))
    (:flag-fk:
     (unicode . "🇫🇰")
     (image . "flag_fk.png")
     (style . "github"))
    (:slot-machine:
     (unicode . "🎰")
     (image . "slot_machine.png")
     (style . "github"))
    (:night-with-stars:
     (unicode . "🌃")
     (image . "night_with_stars.png")
     (style . "github"))
    (:bicyclist-tone4:
     (unicode . "🚴🏾")
     (image . "bicyclist_tone4.png")
     (style . "github"))
    (:aerial_tramway:
     (unicode . "🚡")
     (image . "aerial_tramway.png")
     (style . "github"))
    (:swimmer_tone3:
     (unicode . "🏊🏽")
     (image . "swimmer_tone3.png")
     (style . "github"))
    (:ox:
     (unicode . "🐂")
     (image . "ox.png")
     (style . "github"))
    (:bullettrain_side:
     (unicode . "🚄")
     (image . "bullettrain_side.png")
     (style . "github"))
    (:clock1130:
     (unicode . "🕦")
     (image . "clock1130.png")
     (style . "github"))
    (:santa_tone4:
     (unicode . "🎅🏾")
     (image . "santa_tone4.png")
     (style . "github"))
    (:clubs:
     (unicode . "♣")
     (image . "clubs.png")
     (style . "github"))
    (:construction-site:
     (unicode . "🏗")
     (image . "construction_site.png")
     (style . "github"))
    (:open_file_folder:
     (unicode . "📂")
     (image . "open_file_folder.png")
     (style . "github"))
    (:weight_lifter_tone4:
     (unicode . "🏋🏾")
     (image . "lifter_tone4.png")
     (style . "github"))
    (:nc:
     (unicode . "🇳🇨")
     (image . "flag_nc.png")
     (style . "github"))
    (:flag_gf:
     (unicode . "🇬🇫")
     (image . "flag_gf.png")
     (style . "github"))
    (:dash:
     (unicode . "💨")
     (image . "dash.png")
     (style . "github"))
    (:princess-tone1:
     (unicode . "👸🏻")
     (image . "princess_tone1.png")
     (style . "github"))
    (:dancer-tone2:
     (unicode . "💃🏼")
     (image . "dancer_tone2.png")
     (style . "github"))
    (:put_litter_in_its_place:
     (unicode . "🚮")
     (image . "put_litter_in_its_place.png")
     (style . "github"))
    (:flag-aq:
     (unicode . "🇦🇶")
     (image . "flag_aq.png")
     (style . "github"))
    (:flag_sa:
     (unicode . "🇸🇦")
     (image . "flag_sa.png")
     (style . "github"))
    (:repeat_one:
     (unicode . "🔂")
     (image . "repeat_one.png")
     (style . "github"))
    (:flag-gu:
     (unicode . "🇬🇺")
     (image . "flag_gu.png")
     (style . "github"))
    (:black-large-square:
     (unicode . "⬛")
     (image . "black_large_square.png")
     (style . "github"))
    (:fleur-de-lis:
     (unicode . "⚜")
     (image . "fleur_de_lis.png")
     (style . "github"))
    (:tickets:
     (unicode . "🎟")
     (image . "tickets.png")
     (style . "github"))
    (:shinto-shrine:
     (unicode . "⛩")
     (image . "shinto_shrine.png")
     (style . "github"))
    (:lifter-tone4:
     (unicode . "🏋🏾")
     (image . "lifter_tone4.png")
     (style . "github"))
    (:+1_tone4:
     (unicode . "👍🏾")
     (image . "thumbsup_tone4.png")
     (style . "github"))
    (:smiley:
     (ascii . ":D")
     (unicode . "😃")
     (image . "smiley.png")
     (style . "github"))
    (:writing_hand_tone5:
     (unicode . "✍🏿")
     (image . "writing_hand_tone5.png")
     (style . "github"))
    (:fist_tone4:
     (unicode . "✊🏾")
     (image . "fist_tone4.png")
     (style . "github"))
    (:point_up_tone4:
     (unicode . "☝🏾")
     (image . "point_up_tone4.png")
     (style . "github"))
    (:\'-\)
     (unicode . "😂")
     (image . "joy.png")
     (style . "ascii"))
    (:bell:
     (unicode . "🔔")
     (image . "bell.png")
     (style . "github"))
    (:woman-tone4:
     (unicode . "👩🏾")
     (image . "woman_tone4.png")
     (style . "github"))
    (:urn:
     (unicode . "⚱")
     (image . "urn.png")
     (style . "github"))
    (:open-mouth:
     (ascii . ":-O")
     (unicode . "😮")
     (image . "open_mouth.png")
     (style . "github"))
    (:bride_with_veil_tone2:
     (unicode . "👰🏼")
     (image . "bride_with_veil_tone2.png")
     (style . "github"))
    (:point-up-2:
     (unicode . "👆")
     (image . "point_up_2.png")
     (style . "github"))
    (:guardsman-tone3:
     (unicode . "💂🏽")
     (image . "guardsman_tone3.png")
     (style . "github"))
    (:flag-be:
     (unicode . "🇧🇪")
     (image . "flag_be.png")
     (style . "github"))
    (:flag_bh:
     (unicode . "🇧🇭")
     (image . "flag_bh.png")
     (style . "github"))
    (:cloud_snow:
     (unicode . "🌨")
     (image . "cloud_snow.png")
     (style . "github"))
    (:person_with_pouting_face_tone4:
     (unicode . "🙎🏾")
     (image . "person_with_pouting_face_tone4.png")
     (style . "github"))
    (:punch_tone1:
     (unicode . "👊🏻")
     (image . "punch_tone1.png")
     (style . "github"))
    (:flag_aw:
     (unicode . "🇦🇼")
     (image . "flag_aw.png")
     (style . "github"))
    (:flag_pg:
     (unicode . "🇵🇬")
     (image . "flag_pg.png")
     (style . "github"))
    (:kr:
     (unicode . "🇰🇷")
     (image . "flag_kr.png")
     (style . "github"))
    (:love_letter:
     (unicode . "💌")
     (image . "love_letter.png")
     (style . "github"))
    (:no-good:
     (unicode . "🙅")
     (image . "no_good.png")
     (style . "github"))
    (:no-good-tone3:
     (unicode . "🙅🏽")
     (image . "no_good_tone3.png")
     (style . "github"))
    (:basketball_player_tone4:
     (unicode . "⛹🏾")
     (image . "basketball_player_tone4.png")
     (style . "github"))
    (:horse_racing_tone3:
     (unicode . "🏇🏽")
     (image . "horse_racing_tone3.png")
     (style . "github"))
    (:point_up_tone3:
     (unicode . "☝🏽")
     (image . "point_up_tone3.png")
     (style . "github"))
    (:mosque:
     (unicode . "🕌")
     (image . "mosque.png")
     (style . "github"))
    (:flag-st:
     (unicode . "🇸🇹")
     (image . "flag_st.png")
     (style . "github"))
    (:flag-ao:
     (unicode . "🇦🇴")
     (image . "flag_ao.png")
     (style . "github"))
    (:libra:
     (unicode . "♎")
     (image . "libra.png")
     (style . "github"))
    (:grey-question:
     (unicode . "❔")
     (image . "grey_question.png")
     (style . "github"))
    (:curry:
     (unicode . "🍛")
     (image . "curry.png")
     (style . "github"))
    (:busts_in_silhouette:
     (unicode . "👥")
     (image . "busts_in_silhouette.png")
     (style . "github"))
    (:camping:
     (unicode . "🏕")
     (image . "camping.png")
     (style . "github"))
    (:tropical_fish:
     (unicode . "🐠")
     (image . "tropical_fish.png")
     (style . "github"))
    (:seedling:
     (unicode . "🌱")
     (image . "seedling.png")
     (style . "github"))
    (:shinto_shrine:
     (unicode . "⛩")
     (image . "shinto_shrine.png")
     (style . "github"))
    (:person_with_blond_hair_tone5:
     (unicode . "👱🏿")
     (image . "person_with_blond_hair_tone5.png")
     (style . "github"))
    (:flag_tl:
     (unicode . "🇹🇱")
     (image . "flag_tl.png")
     (style . "github"))
    (:person_with_pouting_face:
     (unicode . "🙎")
     (image . "person_with_pouting_face.png")
     (style . "github"))
    (:point-left-tone4:
     (unicode . "👈🏾")
     (image . "point_left_tone4.png")
     (style . "github"))
    (:ribbon:
     (unicode . "🎀")
     (image . "ribbon.png")
     (style . "github"))
    (:cyclone:
     (unicode . "🌀")
     (image . "cyclone.png")
     (style . "github"))
    (:man_tone4:
     (unicode . "👨🏾")
     (image . "man_tone4.png")
     (style . "github"))
    (:city-sunset:
     (unicode . "🌇")
     (image . "city_sunset.png")
     (style . "github"))
    (:musical_keyboard:
     (unicode . "🎹")
     (image . "musical_keyboard.png")
     (style . "github"))
    (:bow_tone3:
     (unicode . "🙇🏽")
     (image . "bow_tone3.png")
     (style . "github"))
    (:purse:
     (unicode . "👛")
     (image . "purse.png")
     (style . "github"))
    (:u5272:
     (unicode . "🈹")
     (image . "u5272.png")
     (style . "github"))
    (:apple:
     (unicode . "🍎")
     (image . "apple.png")
     (style . "github"))
    (:point-left:
     (unicode . "👈")
     (image . "point_left.png")
     (style . "github"))
    (:basketball:
     (unicode . "🏀")
     (image . "basketball.png")
     (style . "github"))
    (:flag-tl:
     (unicode . "🇹🇱")
     (image . "flag_tl.png")
     (style . "github"))
    (:thumbsdown-tone2:
     (unicode . "👎🏼")
     (image . "thumbsdown_tone2.png")
     (style . "github"))
    (:beach-umbrella:
     (unicode . "⛱")
     (image . "beach_umbrella.png")
     (style . "github"))
    (:page_facing_up:
     (unicode . "📄")
     (image . "page_facing_up.png")
     (style . "github"))
    (:princess-tone2:
     (unicode . "👸🏼")
     (image . "princess_tone2.png")
     (style . "github"))
    (:tractor:
     (unicode . "🚜")
     (image . "tractor.png")
     (style . "github"))
    (:wind_chime:
     (unicode . "🎐")
     (image . "wind_chime.png")
     (style . "github"))
    (:flag-ci:
     (unicode . "🇨🇮")
     (image . "flag_ci.png")
     (style . "github"))
    (:department_store:
     (unicode . "🏬")
     (image . "department_store.png")
     (style . "github"))
    (:flag-is:
     (unicode . "🇮🇸")
     (image . "flag_is.png")
     (style . "github"))
    (:film_projector:
     (unicode . "📽")
     (image . "projector.png")
     (style . "github"))
    (:oden:
     (unicode . "🍢")
     (image . "oden.png")
     (style . "github"))
    (:level-slider:
     (unicode . "🎚")
     (image . "level_slider.png")
     (style . "github"))
    (:smirk_cat:
     (unicode . "😼")
     (image . "smirk_cat.png")
     (style . "github"))
    (:bento:
     (unicode . "🍱")
     (image . "bento.png")
     (style . "github"))
    (:tanabata_tree:
     (unicode . "🎋")
     (image . "tanabata_tree.png")
     (style . "github"))
    (:point_left_tone5:
     (unicode . "👈🏿")
     (image . "point_left_tone5.png")
     (style . "github"))
    (:disappointed-relieved:
     (unicode . "😥")
     (image . "disappointed_relieved.png")
     (style . "github"))
    (:fire-engine:
     (unicode . "🚒")
     (image . "fire_engine.png")
     (style . "github"))
    (:older_man_tone3:
     (unicode . "👴🏽")
     (image . "older_man_tone3.png")
     (style . "github"))
    (:earth_africa:
     (unicode . "🌍")
     (image . "earth_africa.png")
     (style . "github"))
    (:white-circle:
     (unicode . "⚪")
     (image . "white_circle.png")
     (style . "github"))
    (:information-desk-person-tone5:
     (unicode . "💁🏿")
     (image . "information_desk_person_tone5.png")
     (style . "github"))
    (:runner-tone1:
     (unicode . "🏃🏻")
     (image . "runner_tone1.png")
     (style . "github"))
    (:bicyclist_tone4:
     (unicode . "🚴🏾")
     (image . "bicyclist_tone4.png")
     (style . "github"))
    (:walking-tone5:
     (unicode . "🚶🏿")
     (image . "walking_tone5.png")
     (style . "github"))
    (:vulcan_tone5:
     (unicode . "🖖🏿")
     (image . "vulcan_tone5.png")
     (style . "github"))
    (:fireworks:
     (unicode . "🎆")
     (image . "fireworks.png")
     (style . "github"))
    (:file-folder:
     (unicode . "📁")
     (image . "file_folder.png")
     (style . "github"))
    (:flag_cd:
     (unicode . "🇨🇩")
     (image . "flag_cd.png")
     (style . "github"))
    (:crystal_ball:
     (unicode . "🔮")
     (image . "crystal_ball.png")
     (style . "github"))
    (:ballot-box-with-check:
     (unicode . "☑")
     (image . "ballot_box_with_check.png")
     (style . "github"))
    (:flag-im:
     (unicode . "🇮🇲")
     (image . "flag_im.png")
     (style . "github"))
    (:mahjong:
     (unicode . "🀄")
     (image . "mahjong.png")
     (style . "github"))
    (:massage-tone1:
     (unicode . "💆🏻")
     (image . "massage_tone1.png")
     (style . "github"))
    (:ba:
     (unicode . "🇧🇦")
     (image . "flag_ba.png")
     (style . "github"))
    (:sleeping-accommodation:
     (unicode . "🛌")
     (image . "sleeping_accommodation.png")
     (style . "github"))
    (:muscle_tone1:
     (unicode . "💪🏻")
     (image . "muscle_tone1.png")
     (style . "github"))
    (:boy_tone3:
     (unicode . "👦🏽")
     (image . "boy_tone3.png")
     (style . "github"))
    (:spy_tone1:
     (unicode . "🕵🏻")
     (image . "spy_tone1.png")
     (style . "github"))
    (:shit:
     (unicode . "💩")
     (image . "poop.png")
     (style . "github"))
    (:sb:
     (unicode . "🇸🇧")
     (image . "flag_sb.png")
     (style . "github"))
    (:two-hearts:
     (unicode . "💕")
     (image . "two_hearts.png")
     (style . "github"))
    (:taxi:
     (unicode . "🚕")
     (image . "taxi.png")
     (style . "github"))
    (:tuvalu:
     (unicode . "🇹🇻")
     (image . "flag_tv.png")
     (style . "github"))
    (:couch_and_lamp:
     (unicode . "🛋")
     (image . "couch.png")
     (style . "github"))
    (:flag_mk:
     (unicode . "🇲🇰")
     (image . "flag_mk.png")
     (style . "github"))
    (:tent:
     (unicode . "⛺")
     (image . "tent.png")
     (style . "github"))
    (:white_check_mark:
     (unicode . "✅")
     (image . "white_check_mark.png")
     (style . "github"))
    (:walking_tone4:
     (unicode . "🚶🏾")
     (image . "walking_tone4.png")
     (style . "github"))
    (:mailbox-with-mail:
     (unicode . "📬")
     (image . "mailbox_with_mail.png")
     (style . "github"))
    (:point_right_tone5:
     (unicode . "👉🏿")
     (image . "point_right_tone5.png")
     (style . "github"))
    (:person-frowning-tone5:
     (unicode . "🙍🏿")
     (image . "person_frowning_tone5.png")
     (style . "github"))
    (:angel_tone5:
     (unicode . "👼🏿")
     (image . "angel_tone5.png")
     (style . "github"))
    (:worried:
     (unicode . "😟")
     (image . "worried.png")
     (style . "github"))
    (:anger:
     (unicode . "💢")
     (image . "anger.png")
     (style . "github"))
    (:at:
     (unicode . "🇦🇹")
     (image . "flag_at.png")
     (style . "github"))
    (:closed_umbrella:
     (unicode . "🌂")
     (image . "closed_umbrella.png")
     (style . "github"))
    (:raised_hand_with_part_between_middle_and_ring_fingers_tone1:
     (unicode . "🖖🏻")
     (image . "vulcan_tone1.png")
     (style . "github"))
    (:flag_my:
     (unicode . "🇲🇾")
     (image . "flag_my.png")
     (style . "github"))
    (:bicyclist_tone2:
     (unicode . "🚴🏼")
     (image . "bicyclist_tone2.png")
     (style . "github"))
    (:kiss_mm:
     (unicode . "👨❤💋👨")
     (image . "kiss_mm.png")
     (style . "github"))
    (:raised_hand_tone1:
     (unicode . "✋🏻")
     (image . "raised_hand_tone1.png")
     (style . "github"))
    (:nail_care_tone5:
     (unicode . "💅🏿")
     (image . "nail_care_tone5.png")
     (style . "github"))
    (:twisted-rightwards-arrows:
     (unicode . "🔀")
     (image . "twisted_rightwards_arrows.png")
     (style . "github"))
    (:fork-knife-plate:
     (unicode . "🍽")
     (image . "fork_knife_plate.png")
     (style . "github"))
    (:mailbox_closed:
     (unicode . "📪")
     (image . "mailbox_closed.png")
     (style . "github"))
    (:girl_tone3:
     (unicode . "👧🏽")
     (image . "girl_tone3.png")
     (style . "github"))
    (:flag-pr:
     (unicode . "🇵🇷")
     (image . "flag_pr.png")
     (style . "github"))
    (:bowling:
     (unicode . "🎳")
     (image . "bowling.png")
     (style . "github"))
    (:steam-locomotive:
     (unicode . "🚂")
     (image . "steam_locomotive.png")
     (style . "github"))
    (:money_mouth:
     (unicode . "🤑")
     (image . "money_mouth.png")
     (style . "github"))
    (:raised_hand_with_fingers_splayed_tone4:
     (unicode . "🖐🏾")
     (image . "hand_splayed_tone4.png")
     (style . "github"))
    (:flag_zw:
     (unicode . "🇿🇼")
     (image . "flag_zw.png")
     (style . "github"))
    (:orange-book:
     (unicode . "📙")
     (image . "orange_book.png")
     (style . "github"))
    (:restroom:
     (unicode . "🚻")
     (image . "restroom.png")
     (style . "github"))
    (:flag-tz:
     (unicode . "🇹🇿")
     (image . "flag_tz.png")
     (style . "github"))
    (:tf:
     (unicode . "🇹🇫")
     (image . "flag_tf.png")
     (style . "github"))
    (:non-potable-water:
     (unicode . "🚱")
     (image . "non_potable_water.png")
     (style . "github"))
    (:cow2:
     (unicode . "🐄")
     (image . "cow2.png")
     (style . "github"))
    (:ea:
     (unicode . "🇪🇦")
     (image . "flag_ea.png")
     (style . "github"))
    (:black_square_button:
     (unicode . "🔲")
     (image . "black_square_button.png")
     (style . "github"))
    (:point-down-tone4:
     (unicode . "👇🏾")
     (image . "point_down_tone4.png")
     (style . "github"))
    (:^*
     (unicode . "😘")
     (image . "kissing_heart.png")
     (style . "ascii"))
    (:star_of_david:
     (unicode . "✡")
     (image . "star_of_david.png")
     (style . "github"))
    (:flag_lc:
     (unicode . "🇱🇨")
     (image . "flag_lc.png")
     (style . "github"))
    (:mountain-bicyclist-tone3:
     (unicode . "🚵🏽")
     (image . "mountain_bicyclist_tone3.png")
     (style . "github"))
    (:pray-tone3:
     (unicode . "🙏🏽")
     (image . "pray_tone3.png")
     (style . "github"))
    (:flag_nl:
     (unicode . "🇳🇱")
     (image . "flag_nl.png")
     (style . "github"))
    (:sunflower:
     (unicode . "🌻")
     (image . "sunflower.png")
     (style . "github"))
    (:arrows_clockwise:
     (unicode . "🔃")
     (image . "arrows_clockwise.png")
     (style . "github"))
    (:lock_with_ink_pen:
     (unicode . "🔏")
     (image . "lock_with_ink_pen.png")
     (style . "github"))
    (:railway_car:
     (unicode . "🚃")
     (image . "railway_car.png")
     (style . "github"))
    (:white-large-square:
     (unicode . "⬜")
     (image . "white_large_square.png")
     (style . "github"))
    (:golf:
     (unicode . "⛳")
     (image . "golf.png")
     (style . "github"))
    (:person_with_ball_tone2:
     (unicode . "⛹🏼")
     (image . "basketball_player_tone2.png")
     (style . "github"))
    (:yin_yang:
     (unicode . "☯")
     (image . "yin_yang.png")
     (style . "github"))
    (:v-tone2:
     (unicode . "✌🏼")
     (image . "v_tone2.png")
     (style . "github"))
    (:girl-tone5:
     (unicode . "👧🏿")
     (image . "girl_tone5.png")
     (style . "github"))
    (:cx:
     (unicode . "🇨🇽")
     (image . "flag_cx.png")
     (style . "github"))
    (:robot_face:
     (unicode . "🤖")
     (image . "robot.png")
     (style . "github"))
    (:post_office:
     (unicode . "🏣")
     (image . "post_office.png")
     (style . "github"))
    (:baby_tone5:
     (unicode . "👶🏿")
     (image . "baby_tone5.png")
     (style . "github"))
    (:flag_cv:
     (unicode . "🇨🇻")
     (image . "flag_cv.png")
     (style . "github"))
    (:flag-ls:
     (unicode . "🇱🇸")
     (image . "flag_ls.png")
     (style . "github"))
    (:thumbsdown-tone4:
     (unicode . "👎🏾")
     (image . "thumbsdown_tone4.png")
     (style . "github"))
    (:nail-care:
     (unicode . "💅")
     (image . "nail_care.png")
     (style . "github"))
    (:closed-book:
     (unicode . "📕")
     (image . "closed_book.png")
     (style . "github"))
    (:small_red_triangle:
     (unicode . "🔺")
     (image . "small_red_triangle.png")
     (style . "github"))
    (:sun-with-face:
     (unicode . "🌞")
     (image . "sun_with_face.png")
     (style . "github"))
    (:grey_question:
     (unicode . "❔")
     (image . "grey_question.png")
     (style . "github"))
    (:flag-dz:
     (unicode . "🇩🇿")
     (image . "flag_dz.png")
     (style . "github"))
    (:sunglasses:
     (ascii . "B-)")
     (unicode . "😎")
     (image . "sunglasses.png")
     (style . "github"))
    (:bride_with_veil:
     (unicode . "👰")
     (image . "bride_with_veil.png")
     (style . "github"))
    (:point_up_2:
     (unicode . "👆")
     (image . "point_up_2.png")
     (style . "github"))
    (:japanese_goblin:
     (unicode . "👺")
     (image . "japanese_goblin.png")
     (style . "github"))
    (:fallen-leaf:
     (unicode . "🍂")
     (image . "fallen_leaf.png")
     (style . "github"))
    (:mo:
     (unicode . "🇲🇴")
     (image . "flag_mo.png")
     (style . "github"))
    (:cop_tone3:
     (unicode . "👮🏽")
     (image . "cop_tone3.png")
     (style . "github"))
    (:mans-shoe:
     (unicode . "👞")
     (image . "mans_shoe.png")
     (style . "github"))
    (:couple:
     (unicode . "👫")
     (image . "couple.png")
     (style . "github"))
    (:zzz:
     (unicode . "💤")
     (image . "zzz.png")
     (style . "github"))
    (:santa_tone2:
     (unicode . "🎅🏼")
     (image . "santa_tone2.png")
     (style . "github"))
    (:flag-ly:
     (unicode . "🇱🇾")
     (image . "flag_ly.png")
     (style . "github"))
    (:flag-af:
     (unicode . "🇦🇫")
     (image . "flag_af.png")
     (style . "github"))
    (:flag-mf:
     (unicode . "🇲🇫")
     (image . "flag_mf.png")
     (style . "github"))
    (:arrow_right:
     (unicode . "➡")
     (image . "arrow_right.png")
     (style . "github"))
    (:family-mmbb:
     (unicode . "👨👨👦👦")
     (image . "family_mmbb.png")
     (style . "github"))
    (:flag_hm:
     (unicode . "🇭🇲")
     (image . "flag_hm.png")
     (style . "github"))
    (:flag-bt:
     (unicode . "🇧🇹")
     (image . "flag_bt.png")
     (style . "github"))
    (:flag-ug:
     (unicode . "🇺🇬")
     (image . "flag_ug.png")
     (style . "github"))
    (:ear_tone3:
     (unicode . "👂🏽")
     (image . "ear_tone3.png")
     (style . "github"))
    (:person-with-pouting-face-tone5:
     (unicode . "🙎🏿")
     (image . "person_with_pouting_face_tone5.png")
     (style . "github"))
    (:flag_na:
     (unicode . "🇳🇦")
     (image . "flag_na.png")
     (style . "github"))
    (:ear-of-rice:
     (unicode . "🌾")
     (image . "ear_of_rice.png")
     (style . "github"))
    (:flag_gw:
     (unicode . "🇬🇼")
     (image . "flag_gw.png")
     (style . "github"))
    (:co:
     (unicode . "🇨🇴")
     (image . "flag_co.png")
     (style . "github"))
    (:baggage_claim:
     (unicode . "🛄")
     (image . "baggage_claim.png")
     (style . "github"))
    (:hot_dog:
     (unicode . "🌭")
     (image . "hotdog.png")
     (style . "github"))
    (:dk:
     (unicode . "🇩🇰")
     (image . "flag_dk.png")
     (style . "github"))
    (:arrow-double-down:
     (unicode . "⏬")
     (image . "arrow_double_down.png")
     (style . "github"))
    (:flag_sv:
     (unicode . "🇸🇻")
     (image . "flag_sv.png")
     (style . "github"))
    (:stuck_out_tongue_closed_eyes:
     (unicode . "😝")
     (image . "stuck_out_tongue_closed_eyes.png")
     (style . "github"))
    (:ocean:
     (unicode . "🌊")
     (image . "ocean.png")
     (style . "github"))
    (:flag_kg:
     (unicode . "🇰🇬")
     (image . "flag_kg.png")
     (style . "github"))
    (:scorpion:
     (unicode . "🦂")
     (image . "scorpion.png")
     (style . "github"))
    (:christmas_tree:
     (unicode . "🎄")
     (image . "christmas_tree.png")
     (style . "github"))
    (:musical_note:
     (unicode . "🎵")
     (image . "musical_note.png")
     (style . "github"))
    (:flag-nz:
     (unicode . "🇳🇿")
     (image . "flag_nz.png")
     (style . "github"))
    (:flag-mz:
     (unicode . "🇲🇿")
     (image . "flag_mz.png")
     (style . "github"))
    (:man_tone5:
     (unicode . "👨🏿")
     (image . "man_tone5.png")
     (style . "github"))
    (:flag_gi:
     (unicode . "🇬🇮")
     (image . "flag_gi.png")
     (style . "github"))
    (:taurus:
     (unicode . "♉")
     (image . "taurus.png")
     (style . "github"))
    (:chicken:
     (unicode . "🐔")
     (image . "chicken.png")
     (style . "github"))
    (:older-man:
     (unicode . "👴")
     (image . "older_man.png")
     (style . "github"))
    (:point-up:
     (unicode . "☝")
     (image . "point_up.png")
     (style . "github"))
    (:bath-tone1:
     (unicode . "🛀🏻")
     (image . "bath_tone1.png")
     (style . "github"))
    (:gear:
     (unicode . "⚙")
     (image . "gear.png")
     (style . "github"))
    (:notepad-spiral:
     (unicode . "🗒")
     (image . "notepad_spiral.png")
     (style . "github"))
    (:flag_ee:
     (unicode . "🇪🇪")
     (image . "flag_ee.png")
     (style . "github"))
    (:flag-gt:
     (unicode . "🇬🇹")
     (image . "flag_gt.png")
     (style . "github"))
    (:satisfied:
     (ascii . ">:)")
     (unicode . "😆")
     (image . "laughing.png")
     (style . "github"))
    (:gy:
     (unicode . "🇬🇾")
     (image . "flag_gy.png")
     (style . "github"))
    (:flag-eu:
     (unicode . "🇪🇺")
     (image . "flag_eu.png")
     (style . "github"))
    (:point_left_tone3:
     (unicode . "👈🏽")
     (image . "point_left_tone3.png")
     (style . "github"))
    (:skull_and_crossbones:
     (unicode . "☠")
     (image . "skull_crossbones.png")
     (style . "github"))
    (:flag_et:
     (unicode . "🇪🇹")
     (image . "flag_et.png")
     (style . "github"))
    (:+1_tone5:
     (unicode . "👍🏿")
     (image . "thumbsup_tone5.png")
     (style . "github"))
    (:fist_tone5:
     (unicode . "✊🏿")
     (image . "fist_tone5.png")
     (style . "github"))
    (:man_in_business_suit_levitating:
     (unicode . "🕴")
     (image . "levitate.png")
     (style . "github"))
    (:tropical-drink:
     (unicode . "🍹")
     (image . "tropical_drink.png")
     (style . "github"))
    (:raised_hand_with_fingers_splayed:
     (unicode . "🖐")
     (image . "hand_splayed.png")
     (style . "github"))
    (:dancer-tone4:
     (unicode . "💃🏾")
     (image . "dancer_tone4.png")
     (style . "github"))
    (:cloud_with_snow:
     (unicode . "🌨")
     (image . "cloud_snow.png")
     (style . "github"))
    (:cry:
     (ascii . ":'(")
     (unicode . "😢")
     (image . "cry.png")
     (style . "github"))
    (:cactus:
     (unicode . "🌵")
     (image . "cactus.png")
     (style . "github"))
    (:point-right-tone3:
     (unicode . "👉🏽")
     (image . "point_right_tone3.png")
     (style . "github"))
    (:flag-se:
     (unicode . "🇸🇪")
     (image . "flag_se.png")
     (style . "github"))
    (:flag-gb:
     (unicode . "🇬🇧")
     (image . "flag_gb.png")
     (style . "github"))
    (:bar_chart:
     (unicode . "📊")
     (image . "bar_chart.png")
     (style . "github"))
    (:orange_book:
     (unicode . "📙")
     (image . "orange_book.png")
     (style . "github"))
    (:mortar-board:
     (unicode . "🎓")
     (image . "mortar_board.png")
     (style . "github"))
    (:ic:
     (unicode . "🇮🇨")
     (image . "flag_ic.png")
     (style . "github"))
    (:flag-bf:
     (unicode . "🇧🇫")
     (image . "flag_bf.png")
     (style . "github"))
    (:lock:
     (unicode . "🔒")
     (image . "lock.png")
     (style . "github"))
    (:punch_tone2:
     (unicode . "👊🏼")
     (image . "punch_tone2.png")
     (style . "github"))
    (:mountain_bicyclist_tone4:
     (unicode . "🚵🏾")
     (image . "mountain_bicyclist_tone4.png")
     (style . "github"))
    (:open_hands_tone4:
     (unicode . "👐🏾")
     (image . "open_hands_tone4.png")
     (style . "github"))
    (:thunder-cloud-rain:
     (unicode . "⛈")
     (image . "thunder_cloud_rain.png")
     (style . "github"))
    (:v-tone4:
     (unicode . "✌🏾")
     (image . "v_tone4.png")
     (style . "github"))
    (:person_with_ball:
     (unicode . "⛹")
     (image . "basketball_player.png")
     (style . "github"))
    (:bicyclist:
     (unicode . "🚴")
     (image . "bicyclist.png")
     (style . "github"))
    (:flag_jm:
     (unicode . "🇯🇲")
     (image . "flag_jm.png")
     (style . "github"))
    (:bathtub:
     (unicode . "🛁")
     (image . "bathtub.png")
     (style . "github"))
    (:flag_mf:
     (unicode . "🇲🇫")
     (image . "flag_mf.png")
     (style . "github"))
    (:kp:
     (unicode . "🇰🇵")
     (image . "flag_kp.png")
     (style . "github"))
    (:metal-tone3:
     (unicode . "🤘🏽")
     (image . "metal_tone3.png")
     (style . "github"))
    (:wind-chime:
     (unicode . "🎐")
     (image . "wind_chime.png")
     (style . "github"))
    (:track_previous:
     (unicode . "⏮")
     (image . "track_previous.png")
     (style . "github"))
    (:person_with_blond_hair_tone4:
     (unicode . "👱🏾")
     (image . "person_with_blond_hair_tone4.png")
     (style . "github"))
    (:new-moon:
     (unicode . "🌑")
     (image . "new_moon.png")
     (style . "github"))
    (:as:
     (unicode . "🇦🇸")
     (image . "flag_as.png")
     (style . "github"))
    (:flag_tm:
     (unicode . "🇹🇲")
     (image . "flag_tm.png")
     (style . "github"))
    (:palm-tree:
     (unicode . "🌴")
     (image . "palm_tree.png")
     (style . "github"))
    (:point-down:
     (unicode . "👇")
     (image . "point_down.png")
     (style . "github"))
    (:8ball:
     (unicode . "🎱")
     (image . "8ball.png")
     (style . "github"))
    (:hatching-chick:
     (unicode . "🐣")
     (image . "hatching_chick.png")
     (style . "github"))
    (:hand-splayed-tone4:
     (unicode . "🖐🏾")
     (image . "hand_splayed_tone4.png")
     (style . "github"))
    (:tone1:
     (unicode . "🏻")
     (image . "tone1.png")
     (style . "github"))
    (:flag_ag:
     (unicode . "🇦🇬")
     (image . "flag_ag.png")
     (style . "github"))
    (:flag-ec:
     (unicode . "🇪🇨")
     (image . "flag_ec.png")
     (style . "github"))
    (:desert_island:
     (unicode . "🏝")
     (image . "island.png")
     (style . "github"))
    (:hand-splayed:
     (unicode . "🖐")
     (image . "hand_splayed.png")
     (style . "github"))
    (:clapper:
     (unicode . "🎬")
     (image . "clapper.png")
     (style . "github"))
    (:musical-note:
     (unicode . "🎵")
     (image . "musical_note.png")
     (style . "github"))
    (:flag-tm:
     (unicode . "🇹🇲")
     (image . "flag_tm.png")
     (style . "github"))
    (:mouse:
     (unicode . "🐭")
     (image . "mouse.png")
     (style . "github"))
    (:bow_tone5:
     (unicode . "🙇🏿")
     (image . "bow_tone5.png")
     (style . "github"))
    (:princess_tone1:
     (unicode . "👸🏻")
     (image . "princess_tone1.png")
     (style . "github"))
    (:couple-ww:
     (unicode . "👩❤👩")
     (image . "couple_ww.png")
     (style . "github"))
    (:flag-ch:
     (unicode . "🇨🇭")
     (image . "flag_ch.png")
     (style . "github"))
    (:flag-vi:
     (unicode . "🇻🇮")
     (image . "flag_vi.png")
     (style . "github"))
    (:post-office:
     (unicode . "🏣")
     (image . "post_office.png")
     (style . "github"))
    (:flag-ir:
     (unicode . "🇮🇷")
     (image . "flag_ir.png")
     (style . "github"))
    (:house_with_garden:
     (unicode . "🏡")
     (image . "house_with_garden.png")
     (style . "github"))
    (:potable-water:
     (unicode . "🚰")
     (image . "potable_water.png")
     (style . "github"))
    (:person_with_pouting_face_tone3:
     (unicode . "🙎🏽")
     (image . "person_with_pouting_face_tone3.png")
     (style . "github"))
    (:fist-tone3:
     (unicode . "✊🏽")
     (image . "fist_tone3.png")
     (style . "github"))
    (:peace_symbol:
     (unicode . "☮")
     (image . "peace.png")
     (style . "github"))
    (:flag_black:
     (unicode . "🏴")
     (image . "flag_black.png")
     (style . "github"))
    (:ae:
     (unicode . "🇦🇪")
     (image . "flag_ae.png")
     (style . "github"))
    (:flag-kz:
     (unicode . "🇰🇿")
     (image . "flag_kz.png")
     (style . "github"))
    (:princess-tone5:
     (unicode . "👸🏿")
     (image . "princess_tone5.png")
     (style . "github"))
    (:bicyclist_tone5:
     (unicode . "🚴🏿")
     (image . "bicyclist_tone5.png")
     (style . "github"))
    (:vulcan_tone4:
     (unicode . "🖖🏾")
     (image . "vulcan_tone4.png")
     (style . "github"))
    (:walking-tone4:
     (unicode . "🚶🏾")
     (image . "walking_tone4.png")
     (style . "github"))
    (:ge:
     (unicode . "🇬🇪")
     (image . "flag_ge.png")
     (style . "github"))
    (:om:
     (unicode . "🇴🇲")
     (image . "flag_om.png")
     (style . "github"))
    (:camera_with_flash:
     (unicode . "📸")
     (image . "camera_with_flash.png")
     (style . "github"))
    (:ve:
     (unicode . "🇻🇪")
     (image . "flag_ve.png")
     (style . "github"))
    (:nail-care-tone2:
     (unicode . "💅🏼")
     (image . "nail_care_tone2.png")
     (style . "github"))
    (:point_right_tone4:
     (unicode . "👉🏾")
     (image . "point_right_tone4.png")
     (style . "github"))
    (:flag_cg:
     (unicode . "🇨🇬")
     (image . "flag_cg.png")
     (style . "github"))
    (:flag-il:
     (unicode . "🇮🇱")
     (image . "flag_il.png")
     (style . "github"))
    (:football:
     (unicode . "🏈")
     (image . "football.png")
     (style . "github"))
    (:flag-kh:
     (unicode . "🇰🇭")
     (image . "flag_kh.png")
     (style . "github"))
    (:sc:
     (unicode . "🇸🇨")
     (image . "flag_sc.png")
     (style . "github"))
    (:blush:
     (unicode . "😊")
     (image . "blush.png")
     (style . "github"))
    (:hot-pepper:
     (unicode . "🌶")
     (image . "hot_pepper.png")
     (style . "github"))
    (:izakaya-lantern:
     (unicode . "🏮")
     (image . "izakaya_lantern.png")
     (style . "github"))
    (:six:
     (unicode . "6⃣")
     (image . "six.png")
     (style . "github"))
    (:older-woman-tone4:
     (unicode . "👵🏾")
     (image . "older_woman_tone4.png")
     (style . "github"))
    (:record-button:
     (unicode . "⏺")
     (image . "record_button.png")
     (style . "github"))
    (:slight_frown:
     (unicode . "🙁")
     (image . "slight_frown.png")
     (style . "github"))
    (:bath_tone3:
     (unicode . "🛀🏽")
     (image . "bath_tone3.png")
     (style . "github"))
    (:angel_tone4:
     (unicode . "👼🏾")
     (image . "angel_tone4.png")
     (style . "github"))
    (:gb:
     (unicode . "🇬🇧")
     (image . "flag_gb.png")
     (style . "github"))
    (:raised_hand_tone2:
     (unicode . "✋🏼")
     (image . "raised_hand_tone2.png")
     (style . "github"))
    (:raised_hand_with_part_between_middle_and_ring_fingers_tone2:
     (unicode . "🖖🏼")
     (image . "vulcan_tone2.png")
     (style . "github"))
    (:eggplant:
     (unicode . "🍆")
     (image . "eggplant.png")
     (style . "github"))
    (:tada:
     (unicode . "🎉")
     (image . "tada.png")
     (style . "github"))
    (:flag_pa:
     (unicode . "🇵🇦")
     (image . "flag_pa.png")
     (style . "github"))
    (:space_invader:
     (unicode . "👾")
     (image . "space_invader.png")
     (style . "github"))
    (:flag-lr:
     (unicode . "🇱🇷")
     (image . "flag_lr.png")
     (style . "github"))
    (:vulcan-tone2:
     (unicode . "🖖🏼")
     (image . "vulcan_tone2.png")
     (style . "github"))
    (:flag-ps:
     (unicode . "🇵🇸")
     (image . "flag_ps.png")
     (style . "github"))
    (:ru:
     (unicode . "🇷🇺")
     (image . "flag_ru.png")
     (style . "github"))
    (:round_pushpin:
     (unicode . "📍")
     (image . "round_pushpin.png")
     (style . "github"))
    (:raised_hand_with_fingers_splayed_tone3:
     (unicode . "🖐🏽")
     (image . "hand_splayed_tone3.png")
     (style . "github"))
    (:closed_book:
     (unicode . "📕")
     (image . "closed_book.png")
     (style . "github"))
    (:capital-abcd:
     (unicode . "🔠")
     (image . "capital_abcd.png")
     (style . "github"))
    (:flower-playing-cards:
     (unicode . "🎴")
     (image . "flower_playing_cards.png")
     (style . "github"))
    (:ok_woman_tone5:
     (unicode . "🙆🏿")
     (image . "ok_woman_tone5.png")
     (style . "github"))
    (:flag-ru:
     (unicode . "🇷🇺")
     (image . "flag_ru.png")
     (style . "github"))
    (:closed-lock-with-key:
     (unicode . "🔐")
     (image . "closed_lock_with_key.png")
     (style . "github"))
    (:mount_fuji:
     (unicode . "🗻")
     (image . "mount_fuji.png")
     (style . "github"))
    (:waving_white_flag:
     (unicode . "🏳")
     (image . "flag_white.png")
     (style . "github"))
    (:speak_no_evil:
     (unicode . "🙊")
     (image . "speak_no_evil.png")
     (style . "github"))
    (:point_up_2_tone3:
     (unicode . "👆🏽")
     (image . "point_up_2_tone3.png")
     (style . "github"))
    (:panda-face:
     (unicode . "🐼")
     (image . "panda_face.png")
     (style . "github"))
    (:flag-ni:
     (unicode . "🇳🇮")
     (image . "flag_ni.png")
     (style . "github"))
    (:arrows-clockwise:
     (unicode . "🔃")
     (image . "arrows_clockwise.png")
     (style . "github"))
    (:zipper-mouth:
     (unicode . "🤐")
     (image . "zipper_mouth.png")
     (style . "github"))
    (:surfer:
     (unicode . "🏄")
     (image . "surfer.png")
     (style . "github"))
    (:pill:
     (unicode . "💊")
     (image . "pill.png")
     (style . "github"))
    (:sweet-potato:
     (unicode . "🍠")
     (image . "sweet_potato.png")
     (style . "github"))
    (:flag_ug:
     (unicode . "🇺🇬")
     (image . "flag_ug.png")
     (style . "github"))
    (:confused:
     (ascii . ">:\\")
     (unicode . "😕")
     (image . "confused.png")
     (style . "github"))
    (:unlock:
     (unicode . "🔓")
     (image . "unlock.png")
     (style . "github"))
    (:pg:
     (unicode . "🇵🇬")
     (image . "flag_pg.png")
     (style . "github"))
    (:guardsman-tone1:
     (unicode . "💂🏻")
     (image . "guardsman_tone1.png")
     (style . "github"))
    (:volleyball:
     (unicode . "🏐")
     (image . "volleyball.png")
     (style . "github"))
    (:flag_mx:
     (unicode . "🇲🇽")
     (image . "flag_mx.png")
     (style . "github"))
    (:qa:
     (unicode . "🇶🇦")
     (image . "flag_qa.png")
     (style . "github"))
    (:bv:
     (unicode . "🇧🇻")
     (image . "flag_bv.png")
     (style . "github"))
    (:kiss-ww:
     (unicode . "👩❤💋👩")
     (image . "kiss_ww.png")
     (style . "github"))
    (:earth-americas:
     (unicode . "🌎")
     (image . "earth_americas.png")
     (style . "github"))
    (:bicyclist-tone1:
     (unicode . "🚴🏻")
     (image . "bicyclist_tone1.png")
     (style . "github"))
    (:fist:
     (unicode . "✊")
     (image . "fist.png")
     (style . "github"))
    (:european_castle:
     (unicode . "🏰")
     (image . "european_castle.png")
     (style . "github"))
    (:sushi:
     (unicode . "🍣")
     (image . "sushi.png")
     (style . "github"))
    (:fish_cake:
     (unicode . "🍥")
     (image . "fish_cake.png")
     (style . "github"))
    (:bride-with-veil-tone1:
     (unicode . "👰🏻")
     (image . "bride_with_veil_tone1.png")
     (style . "github"))
    (:middle-finger-tone5:
     (unicode . "🖕🏿")
     (image . "middle_finger_tone5.png")
     (style . "github"))
    (:inbox-tray:
     (unicode . "📥")
     (image . "inbox_tray.png")
     (style . "github"))
    (:middle-finger-tone2:
     (unicode . "🖕🏼")
     (image . "middle_finger_tone2.png")
     (style . "github"))
    (:clock630:
     (unicode . "🕡")
     (image . "clock630.png")
     (style . "github"))
    (:projector:
     (unicode . "📽")
     (image . "projector.png")
     (style . "github"))
    (:couplekiss_ww:
     (unicode . "👩❤💋👩")
     (image . "kiss_ww.png")
     (style . "github"))
    (:hotsprings:
     (unicode . "♨")
     (image . "hotsprings.png")
     (style . "github"))
    (:traffic-light:
     (unicode . "🚥")
     (image . "traffic_light.png")
     (style . "github"))
    (:-1_tone2:
     (unicode . "👎🏼")
     (image . "thumbsdown_tone2.png")
     (style . "github"))
    (:ml:
     (unicode . "🇲🇱")
     (image . "flag_ml.png")
     (style . "github"))
    (:performing-arts:
     (unicode . "🎭")
     (image . "performing_arts.png")
     (style . "github"))
    (:hot_pepper:
     (unicode . "🌶")
     (image . "hot_pepper.png")
     (style . "github"))
    (:scream:
     (unicode . "😱")
     (image . "scream.png")
     (style . "github"))
    (:ps:
     (unicode . "🇵🇸")
     (image . "flag_ps.png")
     (style . "github"))
    (:flushed:
     (ascii . ":$")
     (unicode . "😳")
     (image . "flushed.png")
     (style . "github"))
    (:muscle-tone3:
     (unicode . "💪🏽")
     (image . "muscle_tone3.png")
     (style . "github"))
    (:fearful:
     (ascii . "D:")
     (unicode . "😨")
     (image . "fearful.png")
     (style . "github"))
    (:flag_hn:
     (unicode . "🇭🇳")
     (image . "flag_hn.png")
     (style . "github"))
    (:railway-car:
     (unicode . "🚃")
     (image . "railway_car.png")
     (style . "github"))
    (:swimmer_tone5:
     (unicode . "🏊🏿")
     (image . "swimmer_tone5.png")
     (style . "github"))
    (:mz:
     (unicode . "🇲🇿")
     (image . "flag_mz.png")
     (style . "github"))
    (:no-entry:
     (unicode . "⛔")
     (image . "no_entry.png")
     (style . "github"))
    (:dj:
     (unicode . "🇩🇯")
     (image . "flag_dj.png")
     (style . "github"))
    (:flag-mu:
     (unicode . "🇲🇺")
     (image . "flag_mu.png")
     (style . "github"))
    (:japan:
     (unicode . "🗾")
     (image . "japan.png")
     (style . "github"))
    (:ok-hand-tone2:
     (unicode . "👌🏼")
     (image . "ok_hand_tone2.png")
     (style . "github"))
    (:two-women-holding-hands:
     (unicode . "👭")
     (image . "two_women_holding_hands.png")
     (style . "github"))
    (:person_frowning_tone4:
     (unicode . "🙍🏾")
     (image . "person_frowning_tone4.png")
     (style . "github"))
    (:rowboat-tone5:
     (unicode . "🚣🏿")
     (image . "rowboat_tone5.png")
     (style . "github"))
    (:no-mouth:
     (ascii . ":-X")
     (unicode . "😶")
     (image . "no_mouth.png")
     (style . "github"))
    (:indonesia:
     (unicode . "🇮🇩")
     (image . "flag_id.png")
     (style . "github"))
    (:track_next:
     (unicode . "⏭")
     (image . "track_next.png")
     (style . "github"))
    (:thumbsup_tone1:
     (unicode . "👍🏻")
     (image . "thumbsup_tone1.png")
     (style . "github"))
    (:statue_of_liberty:
     (unicode . "🗽")
     (image . "statue_of_liberty.png")
     (style . "github"))
    (:person_with_ball_tone3:
     (unicode . "⛹🏽")
     (image . "basketball_player_tone3.png")
     (style . "github"))
    (:flag_sc:
     (unicode . "🇸🇨")
     (image . "flag_sc.png")
     (style . "github"))
    (:fallen_leaf:
     (unicode . "🍂")
     (image . "fallen_leaf.png")
     (style . "github"))
    (:funeral_urn:
     (unicode . "⚱")
     (image . "urn.png")
     (style . "github"))
    (:ballot_box_with_check:
     (unicode . "☑")
     (image . "ballot_box_with_check.png")
     (style . "github"))
    (:flag-nu:
     (unicode . "🇳🇺")
     (image . "flag_nu.png")
     (style . "github"))
    (:waxing-gibbous-moon:
     (unicode . "🌔")
     (image . "waxing_gibbous_moon.png")
     (style . "github"))
    (:raising_hand_tone5:
     (unicode . "🙋🏿")
     (image . "raising_hand_tone5.png")
     (style . "github"))
    (:flag_mg:
     (unicode . "🇲🇬")
     (image . "flag_mg.png")
     (style . "github"))
    (:page-facing-up:
     (unicode . "📄")
     (image . "page_facing_up.png")
     (style . "github"))
    (:vs:
     (unicode . "🆚")
     (image . "vs.png")
     (style . "github"))
    (:flag-gs:
     (unicode . "🇬🇸")
     (image . "flag_gs.png")
     (style . "github"))
    (:clock730:
     (unicode . "🕢")
     (image . "clock730.png")
     (style . "github"))
    (:anger-right:
     (unicode . "🗯")
     (image . "anger_right.png")
     (style . "github"))
    (:crystal-ball:
     (unicode . "🔮")
     (image . "crystal_ball.png")
     (style . "github"))
    (:crown:
     (unicode . "👑")
     (image . "crown.png")
     (style . "github"))
    (:ir:
     (unicode . "🇮🇷")
     (image . "flag_ir.png")
     (style . "github"))
    (:racing_motorcycle:
     (unicode . "🏍")
     (image . "motorcycle.png")
     (style . "github"))
    (:bridge-at-night:
     (unicode . "🌉")
     (image . "bridge_at_night.png")
     (style . "github"))
    (:flag-mk:
     (unicode . "🇲🇰")
     (image . "flag_mk.png")
     (style . "github"))
    (:six-pointed-star:
     (unicode . "🔯")
     (image . "six_pointed_star.png")
     (style . "github"))
    (:gt:
     (unicode . "🇬🇹")
     (image . "flag_gt.png")
     (style . "github"))
    (:table_tennis:
     (unicode . "🏓")
     (image . "ping_pong.png")
     (style . "github"))
    (:flag-sd:
     (unicode . "🇸🇩")
     (image . "flag_sd.png")
     (style . "github"))
    (:point-right-tone2:
     (unicode . "👉🏼")
     (image . "point_right_tone2.png")
     (style . "github"))
    (:japanese-castle:
     (unicode . "🏯")
     (image . "japanese_castle.png")
     (style . "github"))
    (:mortar_board:
     (unicode . "🎓")
     (image . "mortar_board.png")
     (style . "github"))
    (:flag-ga:
     (unicode . "🇬🇦")
     (image . "flag_ga.png")
     (style . "github"))
    (:no-good-tone1:
     (unicode . "🙅🏻")
     (image . "no_good_tone1.png")
     (style . "github"))
    (:horse-racing-tone5:
     (unicode . "🏇🏿")
     (image . "horse_racing_tone5.png")
     (style . "github"))
    (:flag_bn:
     (unicode . "🇧🇳")
     (image . "flag_bn.png")
     (style . "github"))
    (:punch_tone3:
     (unicode . "👊🏽")
     (image . "punch_tone3.png")
     (style . "github"))
    (:black-medium-square:
     (unicode . "◼")
     (image . "black_medium_square.png")
     (style . "github"))
    (:poultry-leg:
     (unicode . "🍗")
     (image . "poultry_leg.png")
     (style . "github"))
    (:worship_symbol:
     (unicode . "🛐")
     (image . "place_of_worship.png")
     (style . "github"))
    (:mountain_bicyclist_tone5:
     (unicode . "🚵🏿")
     (image . "mountain_bicyclist_tone5.png")
     (style . "github"))
    (:bird:
     (unicode . "🐦")
     (image . "bird.png")
     (style . "github"))
    (:flag-dg:
     (unicode . "🇩🇬")
     (image . "flag_dg.png")
     (style . "github"))
    (:currency_exchange:
     (unicode . "💱")
     (image . "currency_exchange.png")
     (style . "github"))
    (:white_square_button:
     (unicode . "🔳")
     (image . "white_square_button.png")
     (style . "github"))
    (:flag-je:
     (unicode . "🇯🇪")
     (image . "flag_je.png")
     (style . "github"))
    (:cr:
     (unicode . "🇨🇷")
     (image . "flag_cr.png")
     (style . "github"))
    (:uz:
     (unicode . "🇺🇿")
     (image . "flag_uz.png")
     (style . "github"))
    (:flag_tn:
     (unicode . "🇹🇳")
     (image . "flag_tn.png")
     (style . "github"))
    (:negative-squared-cross-mark:
     (unicode . "❎")
     (image . "negative_squared_cross_mark.png")
     (style . "github"))
    (:stew:
     (unicode . "🍲")
     (image . "stew.png")
     (style . "github"))
    (:pizza:
     (unicode . "🍕")
     (image . "pizza.png")
     (style . "github"))
    (:cd:
     (unicode . "💿")
     (image . "cd.png")
     (style . "github"))
    (:flag-sz:
     (unicode . "🇸🇿")
     (image . "flag_sz.png")
     (style . "github"))
    (:blowfish:
     (unicode . "🐡")
     (image . "blowfish.png")
     (style . "github"))
    (:person_with_blond_hair_tone3:
     (unicode . "👱🏽")
     (image . "person_with_blond_hair_tone3.png")
     (style . "github"))
    (:boot:
     (unicode . "👢")
     (image . "boot.png")
     (style . "github"))
    (:low-brightness:
     (unicode . "🔅")
     (image . "low_brightness.png")
     (style . "github"))
    (:wind-blowing-face:
     (unicode . "🌬")
     (image . "wind_blowing_face.png")
     (style . "github"))
    (:flag-uz:
     (unicode . "🇺🇿")
     (image . "flag_uz.png")
     (style . "github"))
    (:flag_hr:
     (unicode . "🇭🇷")
     (image . "flag_hr.png")
     (style . "github"))
    (:family_wwgb:
     (unicode . "👩👩👧👦")
     (image . "family_wwgb.png")
     (style . "github"))
    (:church:
     (unicode . "⛪")
     (image . "church.png")
     (style . "github"))
    (:me:
     (unicode . "🇲🇪")
     (image . "flag_me.png")
     (style . "github"))
    (:flag-cy:
     (unicode . "🇨🇾")
     (image . "flag_cy.png")
     (style . "github"))
    (:hand-splayed-tone5:
     (unicode . "🖐🏿")
     (image . "hand_splayed_tone5.png")
     (style . "github"))
    (:railway_track:
     (unicode . "🛤")
     (image . "railway_track.png")
     (style . "github"))
    (:bath-tone5:
     (unicode . "🛀🏿")
     (image . "bath_tone5.png")
     (style . "github"))
    (:raised-hand-tone1:
     (unicode . "✋🏻")
     (image . "raised_hand_tone1.png")
     (style . "github"))
    (:hourglass_flowing_sand:
     (unicode . "⏳")
     (image . "hourglass_flowing_sand.png")
     (style . "github"))
    (:clock830:
     (unicode . "🕣")
     (image . "clock830.png")
     (style . "github"))
    (:bus:
     (unicode . "🚌")
     (image . "bus.png")
     (style . "github"))
    (:flag-qa:
     (unicode . "🇶🇦")
     (image . "flag_qa.png")
     (style . "github"))
    (:skull_crossbones:
     (unicode . "☠")
     (image . "skull_crossbones.png")
     (style . "github"))
    (:flag-tj:
     (unicode . "🇹🇯")
     (image . "flag_tj.png")
     (style . "github"))
    (:cloud_rain:
     (unicode . "🌧")
     (image . "cloud_rain.png")
     (style . "github"))
    (:comet:
     (unicode . "☄")
     (image . "comet.png")
     (style . "github"))
    (:nine:
     (unicode . "9⃣")
     (image . "nine.png")
     (style . "github"))
    (:black-nib:
     (unicode . "✒")
     (image . "black_nib.png")
     (style . "github"))
    (:flag-co:
     (unicode . "🇨🇴")
     (image . "flag_co.png")
     (style . "github"))
    (:older_woman_tone1:
     (unicode . "👵🏻")
     (image . "older_woman_tone1.png")
     (style . "github"))
    (:writing-hand-tone1:
     (unicode . "✍🏻")
     (image . "writing_hand_tone1.png")
     (style . "github"))
    (:fist-tone4:
     (unicode . "✊🏾")
     (image . "fist_tone4.png")
     (style . "github"))
    (:eight-spoked-asterisk:
     (unicode . "✳")
     (image . "eight_spoked_asterisk.png")
     (style . "github"))
    (:bow_tone4:
     (unicode . "🙇🏾")
     (image . "bow_tone4.png")
     (style . "github"))
    (:information_desk_person:
     (unicode . "💁")
     (image . "information_desk_person.png")
     (style . "github"))
    (:card_index_dividers:
     (unicode . "🗂")
     (image . "dividers.png")
     (style . "github"))
    (:flag-ky:
     (unicode . "🇰🇾")
     (image . "flag_ky.png")
     (style . "github"))
    (:ok_hand_tone4:
     (unicode . "👌🏾")
     (image . "ok_hand_tone4.png")
     (style . "github"))
    (:sr:
     (unicode . "🇸🇷")
     (image . "flag_sr.png")
     (style . "github"))
    (:books:
     (unicode . "📚")
     (image . "books.png")
     (style . "github"))
    (:runner-tone3:
     (unicode . "🏃🏽")
     (image . "runner_tone3.png")
     (style . "github"))
    (:ng:
     (unicode . "🆖")
     (image . "ng.png")
     (style . "github"))
    (:gf:
     (unicode . "🇬🇫")
     (image . "flag_gf.png")
     (style . "github"))
    (:green_book:
     (unicode . "📗")
     (image . "green_book.png")
     (style . "github"))
    (:metal:
     (unicode . "🤘")
     (image . "metal.png")
     (style . "github"))
    (:haircut:
     (unicode . "💇")
     (image . "haircut.png")
     (style . "github"))
    (:guardsman_tone3:
     (unicode . "💂🏽")
     (image . "guardsman_tone3.png")
     (style . "github"))
    (:nail-care-tone3:
     (unicode . "💅🏽")
     (image . "nail_care_tone3.png")
     (style . "github"))
    (:man-with-gua-pi-mao-tone5:
     (unicode . "👲🏿")
     (image . "man_with_gua_pi_mao_tone5.png")
     (style . "github"))
    (:flag_cf:
     (unicode . "🇨🇫")
     (image . "flag_cf.png")
     (style . "github"))
    (:alarm-clock:
     (unicode . "⏰")
     (image . "alarm_clock.png")
     (style . "github"))
    (:rowboat-tone2:
     (unicode . "🚣🏼")
     (image . "rowboat_tone2.png")
     (style . "github"))
    (:ski:
     (unicode . "🎿")
     (image . "ski.png")
     (style . "github"))
    (:speaking_head:
     (unicode . "🗣")
     (image . "speaking_head.png")
     (style . "github"))
    (:sd:
     (unicode . "🇸🇩")
     (image . "flag_sd.png")
     (style . "github"))
    (:muscle_tone3:
     (unicode . "💪🏽")
     (image . "muscle_tone3.png")
     (style . "github"))
    (:boy_tone1:
     (unicode . "👦🏻")
     (image . "boy_tone1.png")
     (style . "github"))
    (:eyes:
     (unicode . "👀")
     (image . "eyes.png")
     (style . "github"))
    (:horse_racing_tone2:
     (unicode . "🏇🏼")
     (image . "horse_racing_tone2.png")
     (style . "github"))
    (:family-mwg:
     (unicode . "👨👩👧")
     (image . "family_mwg.png")
     (style . "github"))
    (:champagne:
     (unicode . "🍾")
     (image . "champagne.png")
     (style . "github"))
    (:no_smoking:
     (unicode . "🚭")
     (image . "no_smoking.png")
     (style . "github"))
    (:bath_tone2:
     (unicode . "🛀🏼")
     (image . "bath_tone2.png")
     (style . "github"))
    (:clock430:
     (unicode . "🕟")
     (image . "clock430.png")
     (style . "github"))
    (:radio:
     (unicode . "📻")
     (image . "radio.png")
     (style . "github"))
    (:police_car:
     (unicode . "🚓")
     (image . "police_car.png")
     (style . "github"))
    (:raised_hand_with_part_between_middle_and_ring_fingers_tone3:
     (unicode . "🖖🏽")
     (image . "vulcan_tone3.png")
     (style . "github"))
    (:raised_hand_tone3:
     (unicode . "✋🏽")
     (image . "raised_hand_tone3.png")
     (style . "github"))
    (:beer:
     (unicode . "🍺")
     (image . "beer.png")
     (style . "github"))
    (:vulcan-tone1:
     (unicode . "🖖🏻")
     (image . "vulcan_tone1.png")
     (style . "github"))
    (:raised_hand:
     (unicode . "✋")
     (image . "raised_hand.png")
     (style . "github"))
    (:woman_tone4:
     (unicode . "👩🏾")
     (image . "woman_tone4.png")
     (style . "github"))
    (:flag_mw:
     (unicode . "🇲🇼")
     (image . "flag_mw.png")
     (style . "github"))
    (:flag_il:
     (unicode . "🇮🇱")
     (image . "flag_il.png")
     (style . "github"))
    (:lifter_tone4:
     (unicode . "🏋🏾")
     (image . "lifter_tone4.png")
     (style . "github"))
    (:sun_with_face:
     (unicode . "🌞")
     (image . "sun_with_face.png")
     (style . "github"))
    (:bullettrain_front:
     (unicode . "🚅")
     (image . "bullettrain_front.png")
     (style . "github"))
    (:house_buildings:
     (unicode . "🏘")
     (image . "homes.png")
     (style . "github"))
    (:vhs:
     (unicode . "📼")
     (image . "vhs.png")
     (style . "github"))
    (:nose_tone3:
     (unicode . "👃🏽")
     (image . "nose_tone3.png")
     (style . "github"))
    (:raised_hands:
     (unicode . "🙌")
     (image . "raised_hands.png")
     (style . "github"))
    (:clap-tone1:
     (unicode . "👏🏻")
     (image . "clap_tone1.png")
     (style . "github"))
    (:point-left-tone2:
     (unicode . "👈🏼")
     (image . "point_left_tone2.png")
     (style . "github"))
    (:flag-io:
     (unicode . "🇮🇴")
     (image . "flag_io.png")
     (style . "github"))
    (:sparkle:
     (unicode . "❇")
     (image . "sparkle.png")
     (style . "github"))
    (:bb:
     (unicode . "🇧🇧")
     (image . "flag_bb.png")
     (style . "github"))
    (:capital_abcd:
     (unicode . "🔠")
     (image . "capital_abcd.png")
     (style . "github"))
    (:je:
     (unicode . "🇯🇪")
     (image . "flag_je.png")
     (style . "github"))
    (:sign_of_the_horns:
     (unicode . "🤘")
     (image . "metal.png")
     (style . "github"))
    (:custard:
     (unicode . "🍮")
     (image . "custard.png")
     (style . "github"))
    (:raised-hands-tone4:
     (unicode . "🙌🏾")
     (image . "raised_hands_tone4.png")
     (style . "github"))
    (:bride-with-veil-tone4:
     (unicode . "👰🏾")
     (image . "bride_with_veil_tone4.png")
     (style . "github"))
    (:surfer-tone5:
     (unicode . "🏄🏿")
     (image . "surfer_tone5.png")
     (style . "github"))
    (:nose-tone4:
     (unicode . "👃🏾")
     (image . "nose_tone4.png")
     (style . "github"))
    (:flag_bb:
     (unicode . "🇧🇧")
     (image . "flag_bb.png")
     (style . "github"))
    (:bullettrain-side:
     (unicode . "🚄")
     (image . "bullettrain_side.png")
     (style . "github"))
    (:earth_asia:
     (unicode . "🌏")
     (image . "earth_asia.png")
     (style . "github"))
    (:flag_bd:
     (unicode . "🇧🇩")
     (image . "flag_bd.png")
     (style . "github"))
    (:arrow-left:
     (unicode . "⬅")
     (image . "arrow_left.png")
     (style . "github"))
    (:flag-tv:
     (unicode . "🇹🇻")
     (image . "flag_tv.png")
     (style . "github"))
    (:mountain-bicyclist-tone1:
     (unicode . "🚵🏻")
     (image . "mountain_bicyclist_tone1.png")
     (style . "github"))
    (:pensive:
     (unicode . "😔")
     (image . "pensive.png")
     (style . "github"))
    (:japanese_castle:
     (unicode . "🏯")
     (image . "japanese_castle.png")
     (style . "github"))
    (:tz:
     (unicode . "🇹🇿")
     (image . "flag_tz.png")
     (style . "github"))
    (:dancer_tone1:
     (unicode . "💃🏻")
     (image . "dancer_tone1.png")
     (style . "github"))
    (:zm:
     (unicode . "🇿🇲")
     (image . "flag_zm.png")
     (style . "github"))
    (:flag_th:
     (unicode . "🇹🇭")
     (image . "flag_th.png")
     (style . "github"))
    (:flag-pa:
     (unicode . "🇵🇦")
     (image . "flag_pa.png")
     (style . "github"))
    (:ok_woman_tone3:
     (unicode . "🙆🏽")
     (image . "ok_woman_tone3.png")
     (style . "github"))
    (:popcorn:
     (unicode . "🍿")
     (image . "popcorn.png")
     (style . "github"))
    (:gift-heart:
     (unicode . "💝")
     (image . "gift_heart.png")
     (style . "github"))
    (:older_man:
     (unicode . "👴")
     (image . "older_man.png")
     (style . "github"))
    (:tired-face:
     (unicode . "😫")
     (image . "tired_face.png")
     (style . "github"))
    (:bicyclist-tone2:
     (unicode . "🚴🏼")
     (image . "bicyclist_tone2.png")
     (style . "github"))
    (:triangular-flag-on-post:
     (unicode . "🚩")
     (image . "triangular_flag_on_post.png")
     (style . "github"))
    (:roller_coaster:
     (unicode . "🎢")
     (image . "roller_coaster.png")
     (style . "github"))
    (:writing_hand:
     (unicode . "✍")
     (image . "writing_hand.png")
     (style . "github"))
    (:rotating_light:
     (unicode . "🚨")
     (image . "rotating_light.png")
     (style . "github"))
    (:evergreen-tree:
     (unicode . "🌲")
     (image . "evergreen_tree.png")
     (style . "github"))
    (:radio-button:
     (unicode . "🔘")
     (image . "radio_button.png")
     (style . "github"))
    (:herb:
     (unicode . "🌿")
     (image . "herb.png")
     (style . "github"))
    (:ok_hand:
     (unicode . "👌")
     (image . "ok_hand.png")
     (style . "github"))
    (:ten:
     (unicode . "🔟")
     (image . "ten.png")
     (style . "github"))
    (:arrow-upper-right:
     (unicode . "↗")
     (image . "arrow_upper_right.png")
     (style . "github"))
    (:running-shirt-with-sash:
     (unicode . "🎽")
     (image . "running_shirt_with_sash.png")
     (style . "github"))
    (:basketball_player_tone5:
     (unicode . "⛹🏿")
     (image . "basketball_player_tone5.png")
     (style . "github"))
    (:lion:
     (unicode . "🦁")
     (image . "lion_face.png")
     (style . "github"))
    (:smoking:
     (unicode . "🚬")
     (image . "smoking.png")
     (style . "github"))
    (:busts-in-silhouette:
     (unicode . "👥")
     (image . "busts_in_silhouette.png")
     (style . "github"))
    (:clock5:
     (unicode . "🕔")
     (image . "clock5.png")
     (style . "github"))
    (:bath_tone1:
     (unicode . "🛀🏻")
     (image . "bath_tone1.png")
     (style . "github"))
    (:nut-and-bolt:
     (unicode . "🔩")
     (image . "nut_and_bolt.png")
     (style . "github"))
    (:motorway:
     (unicode . "🛣")
     (image . "motorway.png")
     (style . "github"))
    (:double_vertical_bar:
     (unicode . "⏸")
     (image . "pause_button.png")
     (style . "github"))
    (:mm:
     (unicode . "🇲🇲")
     (image . "flag_mm.png")
     (style . "github"))
    (:information-desk-person-tone4:
     (unicode . "💁🏾")
     (image . "information_desk_person_tone4.png")
     (style . "github"))
    (:flag-md:
     (unicode . "🇲🇩")
     (image . "flag_md.png")
     (style . "github"))
    (:boy-tone4:
     (unicode . "👦🏾")
     (image . "boy_tone4.png")
     (style . "github"))
    (:flag_ml:
     (unicode . "🇲🇱")
     (image . "flag_ml.png")
     (style . "github"))
    (:flag-pf:
     (unicode . "🇵🇫")
     (image . "flag_pf.png")
     (style . "github"))
    (:pr:
     (unicode . "🇵🇷")
     (image . "flag_pr.png")
     (style . "github"))
    (:cl:
     (unicode . "🆑")
     (image . "cl.png")
     (style . "github"))
    (:princess:
     (unicode . "👸")
     (image . "princess.png")
     (style . "github"))
    (:yen:
     (unicode . "💴")
     (image . "yen.png")
     (style . "github"))
    (:am:
     (unicode . "🇦🇲")
     (image . "flag_am.png")
     (style . "github"))
    (:metal_tone5:
     (unicode . "🤘🏿")
     (image . "metal_tone5.png")
     (style . "github"))
    (:tools:
     (unicode . "🛠")
     (image . "tools.png")
     (style . "github"))
    (:large-blue-diamond:
     (unicode . "🔷")
     (image . "large_blue_diamond.png")
     (style . "github"))
    (:haircut-tone3:
     (unicode . "💇🏽")
     (image . "haircut_tone3.png")
     (style . "github"))
    (:smirk-cat:
     (unicode . "😼")
     (image . "smirk_cat.png")
     (style . "github"))
    (:al:
     (unicode . "🇦🇱")
     (image . "flag_al.png")
     (style . "github"))
    (:dm:
     (unicode . "🇩🇲")
     (image . "flag_dm.png")
     (style . "github"))
    (:flag_gy:
     (unicode . "🇬🇾")
     (image . "flag_gy.png")
     (style . "github"))
    (:blue_car:
     (unicode . "🚙")
     (image . "blue_car.png")
     (style . "github"))
    (:turkey:
     (unicode . "🦃")
     (image . "turkey.png")
     (style . "github"))
    (:flag_no:
     (unicode . "🇳🇴")
     (image . "flag_no.png")
     (style . "github"))
    (:ok-hand-tone3:
     (unicode . "👌🏽")
     (image . "ok_hand_tone3.png")
     (style . "github"))
    (:airplane-arriving:
     (unicode . "🛬")
     (image . "airplane_arriving.png")
     (style . "github"))
    (:pl:
     (unicode . "🇵🇱")
     (image . "flag_pl.png")
     (style . "github"))
    (:ramen:
     (unicode . "🍜")
     (image . "ramen.png")
     (style . "github"))
    (:train:
     (unicode . "🚋")
     (image . "train.png")
     (style . "github"))
    (:flag_eu:
     (unicode . "🇪🇺")
     (image . "flag_eu.png")
     (style . "github"))
    (:grey_exclamation:
     (unicode . "❕")
     (image . "grey_exclamation.png")
     (style . "github"))
    (:spy-tone4:
     (unicode . "🕵🏾")
     (image . "spy_tone4.png")
     (style . "github"))
    (:soon:
     (unicode . "🔜")
     (image . "soon.png")
     (style . "github"))
    (:person_frowning_tone5:
     (unicode . "🙍🏿")
     (image . "person_frowning_tone5.png")
     (style . "github"))
    (:night_with_stars:
     (unicode . "🌃")
     (image . "night_with_stars.png")
     (style . "github"))
    (:mountain-cableway:
     (unicode . "🚠")
     (image . "mountain_cableway.png")
     (style . "github"))
    (:monkey-face:
     (unicode . "🐵")
     (image . "monkey_face.png")
     (style . "github"))
    (:hm:
     (unicode . "🇭🇲")
     (image . "flag_hm.png")
     (style . "github"))
    (:flag_ly:
     (unicode . "🇱🇾")
     (image . "flag_ly.png")
     (style . "github"))
    (:flag_sb:
     (unicode . "🇸🇧")
     (image . "flag_sb.png")
     (style . "github"))
    (:flag_fo:
     (unicode . "🇫🇴")
     (image . "flag_fo.png")
     (style . "github"))
    (:dog2:
     (unicode . "🐕")
     (image . "dog2.png")
     (style . "github"))
    (:purple-heart:
     (unicode . "💜")
     (image . "purple_heart.png")
     (style . "github"))
    (:mushroom:
     (unicode . "🍄")
     (image . "mushroom.png")
     (style . "github"))
    (:ear:
     (unicode . "👂")
     (image . "ear.png")
     (style . "github"))
    (:weight_lifter_tone1:
     (unicode . "🏋🏻")
     (image . "lifter_tone1.png")
     (style . "github"))
    (:flag_ec:
     (unicode . "🇪🇨")
     (image . "flag_ec.png")
     (style . "github"))
    (:laughing:
     (ascii . ">:)")
     (unicode . "😆")
     (image . "laughing.png")
     (style . "github"))
    (:man-tone5:
     (unicode . "👨🏿")
     (image . "man_tone5.png")
     (style . "github"))
    (:no-bell:
     (unicode . "🔕")
     (image . "no_bell.png")
     (style . "github"))
    (:sign_of_the_horns_tone4:
     (unicode . "🤘🏾")
     (image . "metal_tone4.png")
     (style . "github"))
    (:city_dusk:
     (unicode . "🌆")
     (image . "city_dusk.png")
     (style . "github"))
    (:heart:
     (ascii . "<3")
     (unicode . "❤")
     (image . "heart.png")
     (style . "github"))
    (:flag-gr:
     (unicode . "🇬🇷")
     (image . "flag_gr.png")
     (style . "github"))
    (:is:
     (unicode . "🇮🇸")
     (image . "flag_is.png")
     (style . "github"))
    (:point_left_tone1:
     (unicode . "👈🏻")
     (image . "point_left_tone1.png")
     (style . "github"))
    (:bullettrain-front:
     (unicode . "🚅")
     (image . "bullettrain_front.png")
     (style . "github"))
    (:bow-tone4:
     (unicode . "🙇🏾")
     (image . "bow_tone4.png")
     (style . "github"))
    (:arrow_double_up:
     (unicode . "⏫")
     (image . "arrow_double_up.png")
     (style . "github"))
    (:pound:
     (unicode . "💷")
     (image . "pound.png")
     (style . "github"))
    (:runner_tone4:
     (unicode . "🏃🏾")
     (image . "runner_tone4.png")
     (style . "github"))
    (:feet:
     (unicode . "🐾")
     (image . "feet.png")
     (style . "github"))
    (:gu:
     (unicode . "🇬🇺")
     (image . "flag_gu.png")
     (style . "github"))
    (:information-desk-person:
     (unicode . "💁")
     (image . "information_desk_person.png")
     (style . "github"))
    (:o2:
     (unicode . "🅾")
     (image . "o2.png")
     (style . "github"))
    (:flag-gf:
     (unicode . "🇬🇫")
     (image . "flag_gf.png")
     (style . "github"))
    (:flag-sk:
     (unicode . "🇸🇰")
     (image . "flag_sk.png")
     (style . "github"))
    (:alarm_clock:
     (unicode . "⏰")
     (image . "alarm_clock.png")
     (style . "github"))
    (:five:
     (unicode . "5⃣")
     (image . "five.png")
     (style . "github"))
    (:io:
     (unicode . "🇮🇴")
     (image . "flag_io.png")
     (style . "github"))
    (:crossed-swords:
     (unicode . "⚔")
     (image . "crossed_swords.png")
     (style . "github"))
    (:massage_tone5:
     (unicode . "💆🏿")
     (image . "massage_tone5.png")
     (style . "github"))
    (:gift_heart:
     (unicode . "💝")
     (image . "gift_heart.png")
     (style . "github"))
    (:flag-bh:
     (unicode . "🇧🇭")
     (image . "flag_bh.png")
     (style . "github"))
    (:flag_bo:
     (unicode . "🇧🇴")
     (image . "flag_bo.png")
     (style . "github"))
    (:ac:
     (unicode . "🇦🇨")
     (image . "flag_ac.png")
     (style . "github"))
    (:upside_down_face:
     (unicode . "🙃")
     (image . "upside_down.png")
     (style . "github"))
    (:u6307:
     (unicode . "🈯")
     (image . "u6307.png")
     (style . "github"))
    (:dancers:
     (unicode . "👯")
     (image . "dancers.png")
     (style . "github"))
    (:flag-ws:
     (unicode . "🇼🇸")
     (image . "flag_ws.png")
     (style . "github"))
    (:white_sun_behind_cloud:
     (unicode . "🌥")
     (image . "white_sun_cloud.png")
     (style . "github"))
    (:man_with_gua_pi_mao_tone5:
     (unicode . "👲🏿")
     (image . "man_with_gua_pi_mao_tone5.png")
     (style . "github"))
    (:skull-crossbones:
     (unicode . "☠")
     (image . "skull_crossbones.png")
     (style . "github"))
    (:flag-ar:
     (unicode . "🇦🇷")
     (image . "flag_ar.png")
     (style . "github"))
    (:black_large_square:
     (unicode . "⬛")
     (image . "black_large_square.png")
     (style . "github"))
    (:flag-zw:
     (unicode . "🇿🇼")
     (image . "flag_zw.png")
     (style . "github"))
    (:flag_to:
     (unicode . "🇹🇴")
     (image . "flag_to.png")
     (style . "github"))
    (:fork_knife_plate:
     (unicode . "🍽")
     (image . "fork_knife_plate.png")
     (style . "github"))
    (:necktie:
     (unicode . "👔")
     (image . "necktie.png")
     (style . "github"))
    (:waxing-crescent-moon:
     (unicode . "🌒")
     (image . "waxing_crescent_moon.png")
     (style . "github"))
    (:flag_nr:
     (unicode . "🇳🇷")
     (image . "flag_nr.png")
     (style . "github"))
    (:rainbow:
     (unicode . "🌈")
     (image . "rainbow.png")
     (style . "github"))
    (:clock130:
     (unicode . "🕜")
     (image . "clock130.png")
     (style . "github"))
    (:dizzy_face:
     (ascii . "#-)")
     (unicode . "😵")
     (image . "dizzy_face.png")
     (style . "github"))
    (:heart_eyes_cat:
     (unicode . "😻")
     (image . "heart_eyes_cat.png")
     (style . "github"))
    (:aq:
     (unicode . "🇦🇶")
     (image . "flag_aq.png")
     (style . "github"))
    (:telephone:
     (unicode . "☎")
     (image . "telephone.png")
     (style . "github"))
    (:walking_tone3:
     (unicode . "🚶🏽")
     (image . "walking_tone3.png")
     (style . "github"))
    (:pm:
     (unicode . "🇵🇲")
     (image . "flag_pm.png")
     (style . "github"))
    (:person_with_blond_hair_tone2:
     (unicode . "👱🏼")
     (image . "person_with_blond_hair_tone2.png")
     (style . "github"))
    (:flag-cx:
     (unicode . "🇨🇽")
     (image . "flag_cx.png")
     (style . "github"))
    (:cop_tone4:
     (unicode . "👮🏾")
     (image . "cop_tone4.png")
     (style . "github"))
    (:trident:
     (unicode . "🔱")
     (image . "trident.png")
     (style . "github"))
    (:bride_with_veil_tone1:
     (unicode . "👰🏻")
     (image . "bride_with_veil_tone1.png")
     (style . "github"))
    (:flag_ye:
     (unicode . "🇾🇪")
     (image . "flag_ye.png")
     (style . "github"))
    (:bath-tone4:
     (unicode . "🛀🏾")
     (image . "bath_tone4.png")
     (style . "github"))
    (:ke:
     (unicode . "🇰🇪")
     (image . "flag_ke.png")
     (style . "github"))
    (:dagger:
     (unicode . "🗡")
     (image . "dagger.png")
     (style . "github"))
    (:point-up-2-tone5:
     (unicode . "👆🏿")
     (image . "point_up_2_tone5.png")
     (style . "github"))
    (:dragon_face:
     (unicode . "🐲")
     (image . "dragon_face.png")
     (style . "github"))
    (:raised-hand-tone2:
     (unicode . "✋🏼")
     (image . "raised_hand_tone2.png")
     (style . "github"))
    (:ballot_box_with_ballot:
     (unicode . "🗳")
     (image . "ballot_box.png")
     (style . "github"))
    (:credit_card:
     (unicode . "💳")
     (image . "credit_card.png")
     (style . "github"))
    (:archery:
     (unicode . "🏹")
     (image . "bow_and_arrow.png")
     (style . "github"))
    (:princess_tone3:
     (unicode . "👸🏽")
     (image . "princess_tone3.png")
     (style . "github"))
    (:fist-tone5:
     (unicode . "✊🏿")
     (image . "fist_tone5.png")
     (style . "github"))
    (:flag-tk:
     (unicode . "🇹🇰")
     (image . "flag_tk.png")
     (style . "github"))
    (:notebook:
     (unicode . "📓")
     (image . "notebook.png")
     (style . "github"))
    (:suspension_railway:
     (unicode . "🚟")
     (image . "suspension_railway.png")
     (style . "github"))
    (:zero:
     (unicode . "0⃣")
     (image . "zero.png")
     (style . "github"))
    (:shell:
     (unicode . "🐚")
     (image . "shell.png")
     (style . "github"))
    (:writing-hand-tone2:
     (unicode . "✍🏼")
     (image . "writing_hand_tone2.png")
     (style . "github"))
    (:angel_tone1:
     (unicode . "👼🏻")
     (image . "angel_tone1.png")
     (style . "github"))
    (:sweat-drops:
     (unicode . "💦")
     (image . "sweat_drops.png")
     (style . "github"))
    (:surfer_tone5:
     (unicode . "🏄🏿")
     (image . "surfer_tone5.png")
     (style . "github"))
    (:map:
     (unicode . "🗺")
     (image . "map.png")
     (style . "github"))
    (:hugging:
     (unicode . "🤗")
     (image . "hugging.png")
     (style . "github"))
    (:flag-cn:
     (unicode . "🇨🇳")
     (image . "flag_cn.png")
     (style . "github"))
    (:no_bicycles:
     (unicode . "🚳")
     (image . "no_bicycles.png")
     (style . "github"))
    (:timer:
     (unicode . "⏲")
     (image . "timer.png")
     (style . "github"))
    (:clock1230:
     (unicode . "🕧")
     (image . "clock1230.png")
     (style . "github"))
    (:nl:
     (unicode . "🇳🇱")
     (image . "flag_nl.png")
     (style . "github"))
    (:mens:
     (unicode . "🚹")
     (image . "mens.png")
     (style . "github"))
    (:pig-nose:
     (unicode . "🐽")
     (image . "pig_nose.png")
     (style . "github"))
    (:kiss_ww:
     (unicode . "👩❤💋👩")
     (image . "kiss_ww.png")
     (style . "github"))
    (:elephant:
     (unicode . "🐘")
     (image . "elephant.png")
     (style . "github"))
    (:thermometer:
     (unicode . "🌡")
     (image . "thermometer.png")
     (style . "github"))
    (:ss:
     (unicode . "🇸🇸")
     (image . "flag_ss.png")
     (style . "github"))
    (:ok_hand_tone5:
     (unicode . "👌🏿")
     (image . "ok_hand_tone5.png")
     (style . "github"))
    (:crying-cat-face:
     (unicode . "😿")
     (image . "crying_cat_face.png")
     (style . "github"))
    (:atm:
     (unicode . "🏧")
     (image . "atm.png")
     (style . "github"))
    (:flag-sy:
     (unicode . "🇸🇾")
     (image . "flag_sy.png")
     (style . "github"))
    (:gg:
     (unicode . "🇬🇬")
     (image . "flag_gg.png")
     (style . "github"))
    (:flag-gn:
     (unicode . "🇬🇳")
     (image . "flag_gn.png")
     (style . "github"))
    (:leaves:
     (unicode . "🍃")
     (image . "leaves.png")
     (style . "github"))
    (:flag_dk:
     (unicode . "🇩🇰")
     (image . "flag_dk.png")
     (style . "github"))
    (:dragon-face:
     (unicode . "🐲")
     (image . "dragon_face.png")
     (style . "github"))
    (:clock1:
     (unicode . "🕐")
     (image . "clock1.png")
     (style . "github"))
    (:vc:
     (unicode . "🇻🇨")
     (image . "flag_vc.png")
     (style . "github"))
    (:man:
     (unicode . "👨")
     (image . "man.png")
     (style . "github"))
    (:spy_tone4:
     (unicode . "🕵🏾")
     (image . "spy_tone4.png")
     (style . "github"))
    (:bf:
     (unicode . "🇧🇫")
     (image . "flag_bf.png")
     (style . "github"))
    (:aries:
     (unicode . "♈")
     (image . "aries.png")
     (style . "github"))
    (:flag_ca:
     (unicode . "🇨🇦")
     (image . "flag_ca.png")
     (style . "github"))
    (:hourglass:
     (unicode . "⌛")
     (image . "hourglass.png")
     (style . "github"))
    (:flag_mh:
     (unicode . "🇲🇭")
     (image . "flag_mh.png")
     (style . "github"))
    (:se:
     (unicode . "🇸🇪")
     (image . "flag_se.png")
     (style . "github"))
    (:joystick:
     (unicode . "🕹")
     (image . "joystick.png")
     (style . "github"))
    (:muscle_tone2:
     (unicode . "💪🏼")
     (image . "muscle_tone2.png")
     (style . "github"))
    (:cherry_blossom:
     (unicode . "🌸")
     (image . "cherry_blossom.png")
     (style . "github"))
    (:white_medium_small_square:
     (unicode . "◽")
     (image . "white_medium_small_square.png")
     (style . "github"))
    (:video_camera:
     (unicode . "📹")
     (image . "video_camera.png")
     (style . "github"))
    (:construction_site:
     (unicode . "🏗")
     (image . "construction_site.png")
     (style . "github"))
    (:surfer_tone4:
     (unicode . "🏄🏾")
     (image . "surfer_tone4.png")
     (style . "github"))
    (:hatched-chick:
     (unicode . "🐥")
     (image . "hatched_chick.png")
     (style . "github"))
    (:girl_tone2:
     (unicode . "👧🏼")
     (image . "girl_tone2.png")
     (style . "github"))
    (:airplane_small:
     (unicode . "🛩")
     (image . "airplane_small.png")
     (style . "github"))
    (:flag_bs:
     (unicode . "🇧🇸")
     (image . "flag_bs.png")
     (style . "github"))
    (:rugby-football:
     (unicode . "🏉")
     (image . "rugby_football.png")
     (style . "github"))
    (:left-luggage:
     (unicode . "🛅")
     (image . "left_luggage.png")
     (style . "github"))
    (:woman_tone5:
     (unicode . "👩🏿")
     (image . "woman_tone5.png")
     (style . "github"))
    (:raised_hand_tone4:
     (unicode . "✋🏾")
     (image . "raised_hand_tone4.png")
     (style . "github"))
    (:wave-tone1:
     (unicode . "👋🏻")
     (image . "wave_tone1.png")
     (style . "github"))
    (:dancer_tone4:
     (unicode . "💃🏾")
     (image . "dancer_tone4.png")
     (style . "github"))
    (:heartpulse:
     (unicode . "💗")
     (image . "heartpulse.png")
     (style . "github"))
    (:lifter_tone5:
     (unicode . "🏋🏿")
     (image . "lifter_tone5.png")
     (style . "github"))
    (:ideograph-advantage:
     (unicode . "🉐")
     (image . "ideograph_advantage.png")
     (style . "github"))
    (:bookmark:
     (unicode . "🔖")
     (image . "bookmark.png")
     (style . "github"))
    (:rice-scene:
     (unicode . "🎑")
     (image . "rice_scene.png")
     (style . "github"))
    (:flag-li:
     (unicode . "🇱🇮")
     (image . "flag_li.png")
     (style . "github"))
    (:rage:
     (unicode . "😡")
     (image . "rage.png")
     (style . "github"))
    (:clap-tone2:
     (unicode . "👏🏼")
     (image . "clap_tone2.png")
     (style . "github"))
    (:crab:
     (unicode . "🦀")
     (image . "crab.png")
     (style . "github"))
    (:house_abandoned:
     (unicode . "🏚")
     (image . "house_abandoned.png")
     (style . "github"))
    (:tk:
     (unicode . "🇹🇰")
     (image . "flag_tk.png")
     (style . "github"))
    (:flag-in:
     (unicode . "🇮🇳")
     (image . "flag_in.png")
     (style . "github"))
    (:\'-\(
     (unicode . "😢")
     (image . "cry.png")
     (style . "ascii"))
    (:milky-way:
     (unicode . "🌌")
     (image . "milky_way.png")
     (style . "github"))
    (:zap:
     (unicode . "⚡")
     (image . "zap.png")
     (style . "github"))
    (:flag-rw:
     (unicode . "🇷🇼")
     (image . "flag_rw.png")
     (style . "github"))
    (:gem:
     (unicode . "💎")
     (image . "gem.png")
     (style . "github"))
    (:heavy-multiplication-x:
     (unicode . "✖")
     (image . "heavy_multiplication_x.png")
     (style . "github"))
    (:point_up_2_tone5:
     (unicode . "👆🏿")
     (image . "point_up_2_tone5.png")
     (style . "github"))
    (:vulcan_tone1:
     (unicode . "🖖🏻")
     (image . "vulcan_tone1.png")
     (style . "github"))
    (:park:
     (unicode . "🏞")
     (image . "park.png")
     (style . "github"))
    (:white_medium_square:
     (unicode . "◻")
     (image . "white_medium_square.png")
     (style . "github"))
    (:flag_ax:
     (unicode . "🇦🇽")
     (image . "flag_ax.png")
     (style . "github"))
    (:-1_tone4:
     (unicode . "👎🏾")
     (image . "thumbsdown_tone4.png")
     (style . "github"))
    (:flag-tw:
     (unicode . "🇹🇼")
     (image . "flag_tw.png")
     (style . "github"))
    (:flag_gh:
     (unicode . "🇬🇭")
     (image . "flag_gh.png")
     (style . "github"))
    (:point-down-tone1:
     (unicode . "👇🏻")
     (image . "point_down_tone1.png")
     (style . "github"))
    (:loud_sound:
     (unicode . "🔊")
     (image . "loud_sound.png")
     (style . "github"))
    (:tea:
     (unicode . "🍵")
     (image . "tea.png")
     (style . "github"))
    (:name-badge:
     (unicode . "📛")
     (image . "name_badge.png")
     (style . "github"))
    (:flag_rs:
     (unicode . "🇷🇸")
     (image . "flag_rs.png")
     (style . "github"))
    (:large-blue-circle:
     (unicode . "🔵")
     (image . "large_blue_circle.png")
     (style . "github"))
    (:desert:
     (unicode . "🏜")
     (image . "desert.png")
     (style . "github"))
    (:person_with_ball_tone1:
     (unicode . "⛹🏻")
     (image . "basketball_player_tone1.png")
     (style . "github"))
    (:bicyclist-tone3:
     (unicode . "🚴🏽")
     (image . "bicyclist_tone3.png")
     (style . "github"))
    (:bt:
     (unicode . "🇧🇹")
     (image . "flag_bt.png")
     (style . "github"))
    (:older-man-tone4:
     (unicode . "👴🏾")
     (image . "older_man_tone4.png")
     (style . "github"))
    (:electric-plug:
     (unicode . "🔌")
     (image . "electric_plug.png")
     (style . "github"))
    (:first-quarter-moon:
     (unicode . "🌓")
     (image . "first_quarter_moon.png")
     (style . "github"))
    (:u7533:
     (unicode . "🈸")
     (image . "u7533.png")
     (style . "github"))
    (:person-with-blond-hair-tone4:
     (unicode . "👱🏾")
     (image . "person_with_blond_hair_tone4.png")
     (style . "github"))
    (:flag_it:
     (unicode . "🇮🇹")
     (image . "flag_it.png")
     (style . "github"))
    (:sleuth_or_spy_tone2:
     (unicode . "🕵🏼")
     (image . "spy_tone2.png")
     (style . "github"))
    (:synagogue:
     (unicode . "🕍")
     (image . "synagogue.png")
     (style . "github"))
    (:twisted_rightwards_arrows:
     (unicode . "🔀")
     (image . "twisted_rightwards_arrows.png")
     (style . "github"))
    (:tanabata-tree:
     (unicode . "🎋")
     (image . "tanabata_tree.png")
     (style . "github"))
    (:sleeping_accommodation:
     (unicode . "🛌")
     (image . "sleeping_accommodation.png")
     (style . "github"))
    (:hushed:
     (unicode . "😯")
     (image . "hushed.png")
     (style . "github"))
    (:national_park:
     (unicode . "🏞")
     (image . "park.png")
     (style . "github"))
    (:exclamation:
     (unicode . "❗")
     (image . "exclamation.png")
     (style . "github"))
    (:ne:
     (unicode . "🇳🇪")
     (image . "flag_ne.png")
     (style . "github"))
    (:bz:
     (unicode . "🇧🇿")
     (image . "flag_bz.png")
     (style . "github"))
    (:spy_tone3:
     (unicode . "🕵🏽")
     (image . "spy_tone3.png")
     (style . "github"))
    (:flag_us:
     (unicode . "🇺🇸")
     (image . "flag_us.png")
     (style . "github"))
    (:person-with-pouting-face-tone4:
     (unicode . "🙎🏾")
     (image . "person_with_pouting_face_tone4.png")
     (style . "github"))
    (:kaaba:
     (unicode . "🕋")
     (image . "kaaba.png")
     (style . "github"))
    (:clock4:
     (unicode . "🕓")
     (image . "clock4.png")
     (style . "github"))
    (:hand_splayed_tone4:
     (unicode . "🖐🏾")
     (image . "hand_splayed_tone4.png")
     (style . "github"))
    (:stars:
     (unicode . "🌠")
     (image . "stars.png")
     (style . "github"))
    (:dz:
     (unicode . "🇩🇿")
     (image . "flag_dz.png")
     (style . "github"))
    (:cocktail:
     (unicode . "🍸")
     (image . "cocktail.png")
     (style . "github"))
    (:penguin:
     (unicode . "🐧")
     (image . "penguin.png")
     (style . "github"))
    (:construction_worker_tone4:
     (unicode . "👷🏾")
     (image . "construction_worker_tone4.png")
     (style . "github"))
    (:boy-tone5:
     (unicode . "👦🏿")
     (image . "boy_tone5.png")
     (style . "github"))
    (:basketball-player-tone3:
     (unicode . "⛹🏽")
     (image . "basketball_player_tone3.png")
     (style . "github"))
    (:flag-pg:
     (unicode . "🇵🇬")
     (image . "flag_pg.png")
     (style . "github"))
    (:fr:
     (unicode . "🇫🇷")
     (image . "flag_fr.png")
     (style . "github"))
    (:flag-za:
     (unicode . "🇿🇦")
     (image . "flag_za.png")
     (style . "github"))
    (:fried-shrimp:
     (unicode . "🍤")
     (image . "fried_shrimp.png")
     (style . "github"))
    (:ok-woman-tone1:
     (unicode . "🙆🏻")
     (image . "ok_woman_tone1.png")
     (style . "github"))
    (:no_bell:
     (unicode . "🔕")
     (image . "no_bell.png")
     (style . "github"))
    (:new:
     (unicode . "🆕")
     (image . "new.png")
     (style . "github"))
    (:unicorn_face:
     (unicode . "🦄")
     (image . "unicorn.png")
     (style . "github"))
    (:station:
     (unicode . "🚉")
     (image . "station.png")
     (style . "github"))
    (:flag-ac:
     (unicode . "🇦🇨")
     (image . "flag_ac.png")
     (style . "github"))
    (:beetle:
     (unicode . "🐞")
     (image . "beetle.png")
     (style . "github"))
    (:mx:
     (unicode . "🇲🇽")
     (image . "flag_mx.png")
     (style . "github"))
    (:airplane_arriving:
     (unicode . "🛬")
     (image . "airplane_arriving.png")
     (style . "github"))
    (:flag_ss:
     (unicode . "🇸🇸")
     (image . "flag_ss.png")
     (style . "github"))
    (:snail:
     (unicode . "🐌")
     (image . "snail.png")
     (style . "github"))
    (:lollipop:
     (unicode . "🍭")
     (image . "lollipop.png")
     (style . "github"))
    (:upside_down:
     (unicode . "🙃")
     (image . "upside_down.png")
     (style . "github"))
    (:flag_lb:
     (unicode . "🇱🇧")
     (image . "flag_lb.png")
     (style . "github"))
    (:movie_camera:
     (unicode . "🎥")
     (image . "movie_camera.png")
     (style . "github"))
    (:ro:
     (unicode . "🇷🇴")
     (image . "flag_ro.png")
     (style . "github"))
    (:cloud_tornado:
     (unicode . "🌪")
     (image . "cloud_tornado.png")
     (style . "github"))
    (:person-with-pouting-face:
     (unicode . "🙎")
     (image . "person_with_pouting_face.png")
     (style . "github"))
    (:thumbsup_tone3:
     (unicode . "👍🏽")
     (image . "thumbsup_tone3.png")
     (style . "github"))
    (:umbrella2:
     (unicode . "☂")
     (image . "umbrella2.png")
     (style . "github"))
    (:flag_sm:
     (unicode . "🇸🇲")
     (image . "flag_sm.png")
     (style . "github"))
    (:two:
     (unicode . "2⃣")
     (image . "two.png")
     (style . "github"))
    (:lock-with-ink-pen:
     (unicode . "🔏")
     (image . "lock_with_ink_pen.png")
     (style . "github"))
    (:flag_pr:
     (unicode . "🇵🇷")
     (image . "flag_pr.png")
     (style . "github"))
    (:minidisc:
     (unicode . "💽")
     (image . "minidisc.png")
     (style . "github"))
    (:pig2:
     (unicode . "🐖")
     (image . "pig2.png")
     (style . "github"))
    (:ms:
     (unicode . "🇲🇸")
     (image . "flag_ms.png")
     (style . "github"))
    (:flag-cw:
     (unicode . "🇨🇼")
     (image . "flag_cw.png")
     (style . "github"))
    (:dancer-tone5:
     (unicode . "💃🏿")
     (image . "dancer_tone5.png")
     (style . "github"))
    (:de:
     (unicode . "🇩🇪")
     (image . "flag_de.png")
     (style . "github"))
    (:thunder_cloud_and_rain:
     (unicode . "⛈")
     (image . "thunder_cloud_rain.png")
     (style . "github"))
    (:spy_tone5:
     (unicode . "🕵🏿")
     (image . "spy_tone5.png")
     (style . "github"))
    (:curly-loop:
     (unicode . "➰")
     (image . "curly_loop.png")
     (style . "github"))
    (:bouquet:
     (unicode . "💐")
     (image . "bouquet.png")
     (style . "github"))
    (:stop-button:
     (unicode . "⏹")
     (image . "stop_button.png")
     (style . "github"))
    (:flag-gq:
     (unicode . "🇬🇶")
     (image . "flag_gq.png")
     (style . "github"))
    (:hear_no_evil:
     (unicode . "🙉")
     (image . "hear_no_evil.png")
     (style . "github"))
    (:writing_hand_tone1:
     (unicode . "✍🏻")
     (image . "writing_hand_tone1.png")
     (style . "github"))
    (:flag-bw:
     (unicode . "🇧🇼")
     (image . "flag_bw.png")
     (style . "github"))
    (:full-moon:
     (unicode . "🌕")
     (image . "full_moon.png")
     (style . "github"))
    (:cheese:
     (unicode . "🧀")
     (image . "cheese.png")
     (style . "github"))
    (:warning:
     (unicode . "⚠")
     (image . "warning.png")
     (style . "github"))
    (:bow-tone5:
     (unicode . "🙇🏿")
     (image . "bow_tone5.png")
     (style . "github"))
    (:bow_and_arrow:
     (unicode . "🏹")
     (image . "bow_and_arrow.png")
     (style . "github"))
    (:reversed_hand_with_middle_finger_extended_tone2:
     (unicode . "🖕🏼")
     (image . "middle_finger_tone2.png")
     (style . "github"))
    (:runner_tone5:
     (unicode . "🏃🏿")
     (image . "runner_tone5.png")
     (style . "github"))
    (:black-circle:
     (unicode . "⚫")
     (image . "black_circle.png")
     (style . "github"))
    (:womans_clothes:
     (unicode . "👚")
     (image . "womans_clothes.png")
     (style . "github"))
    (:flag_dz:
     (unicode . "🇩🇿")
     (image . "flag_dz.png")
     (style . "github"))
    (:family-mmg:
     (unicode . "👨👨👧")
     (image . "family_mmg.png")
     (style . "github"))
    (:gw:
     (unicode . "🇬🇼")
     (image . "flag_gw.png")
     (style . "github"))
    (:droplet:
     (unicode . "💧")
     (image . "droplet.png")
     (style . "github"))
    (:cop-tone4:
     (unicode . "👮🏾")
     (image . "cop_tone4.png")
     (style . "github"))
    (:horse_racing:
     (unicode . "🏇")
     (image . "horse_racing.png")
     (style . "github"))
    (:horse-racing:
     (unicode . "🏇")
     (image . "horse_racing.png")
     (style . "github"))
    (:dromedary_camel:
     (unicode . "🐪")
     (image . "dromedary_camel.png")
     (style . "github"))
    (:flag-sj:
     (unicode . "🇸🇯")
     (image . "flag_sj.png")
     (style . "github"))
    (:massage_tone4:
     (unicode . "💆🏾")
     (image . "massage_tone4.png")
     (style . "github"))
    (:flag-am:
     (unicode . "🇦🇲")
     (image . "flag_am.png")
     (style . "github"))
    (:cinema:
     (unicode . "🎦")
     (image . "cinema.png")
     (style . "github"))
    (:dizzy:
     (unicode . "💫")
     (image . "dizzy.png")
     (style . "github"))
    (:confetti_ball:
     (unicode . "🎊")
     (image . "confetti_ball.png")
     (style . "github"))
    (:dolphin:
     (unicode . "🐬")
     (image . "dolphin.png")
     (style . "github"))
    (:ballot_box:
     (unicode . "🗳")
     (image . "ballot_box.png")
     (style . "github"))
    (:flag-bi:
     (unicode . "🇧🇮")
     (image . "flag_bi.png")
     (style . "github"))
    (:non-potable_water:
     (unicode . "🚱")
     (image . "non_potable_water.png")
     (style . "github"))
    (:flag_kr:
     (unicode . "🇰🇷")
     (image . "flag_kr.png")
     (style . "github"))
    (:atom_symbol:
     (unicode . "⚛")
     (image . "atom.png")
     (style . "github"))
    (:flag_nz:
     (unicode . "🇳🇿")
     (image . "flag_nz.png")
     (style . "github"))
    (:heavy-dollar-sign:
     (unicode . "💲")
     (image . "heavy_dollar_sign.png")
     (style . "github"))
    (:flag-de:
     (unicode . "🇩🇪")
     (image . "flag_de.png")
     (style . "github"))
    (:man_with_gua_pi_mao_tone4:
     (unicode . "👲🏾")
     (image . "man_with_gua_pi_mao_tone4.png")
     (style . "github"))
    (:envelope-with-arrow:
     (unicode . "📩")
     (image . "envelope_with_arrow.png")
     (style . "github"))
    (:anguished:
     (unicode . "😧")
     (image . "anguished.png")
     (style . "github"))
    (:flag-wf:
     (unicode . "🇼🇫")
     (image . "flag_wf.png")
     (style . "github"))
    (:pouting-cat:
     (unicode . "😾")
     (image . "pouting_cat.png")
     (style . "github"))
    (:ye:
     (unicode . "🇾🇪")
     (image . "flag_ye.png")
     (style . "github"))
    (:heartbeat:
     (unicode . "💓")
     (image . "heartbeat.png")
     (style . "github"))
    (:poodle:
     (unicode . "🐩")
     (image . "poodle.png")
     (style . "github"))
    (:u6709:
     (unicode . "🈶")
     (image . "u6709.png")
     (style . "github"))
    (:triangular-ruler:
     (unicode . "📐")
     (image . "triangular_ruler.png")
     (style . "github"))
    (:no-mobile-phones:
     (unicode . "📵")
     (image . "no_mobile_phones.png")
     (style . "github"))
    (:person_with_blond_hair_tone1:
     (unicode . "👱🏻")
     (image . "person_with_blond_hair_tone1.png")
     (style . "github"))
    (:open_hands_tone3:
     (unicode . "👐🏽")
     (image . "open_hands_tone3.png")
     (style . "github"))
    (:flag_ht:
     (unicode . "🇭🇹")
     (image . "flag_ht.png")
     (style . "github"))
    (:muscle_tone5:
     (unicode . "💪🏿")
     (image . "muscle_tone5.png")
     (style . "github"))
    (:cop_tone5:
     (unicode . "👮🏿")
     (image . "cop_tone5.png")
     (style . "github"))
    (:flag_at:
     (unicode . "🇦🇹")
     (image . "flag_at.png")
     (style . "github"))
    (:point_up_2_tone1:
     (unicode . "👆🏻")
     (image . "point_up_2_tone1.png")
     (style . "github"))
    (:hamburger:
     (unicode . "🍔")
     (image . "hamburger.png")
     (style . "github"))
    (:rabbit2:
     (unicode . "🐇")
     (image . "rabbit2.png")
     (style . "github"))
    (:smile-cat:
     (unicode . "😸")
     (image . "smile_cat.png")
     (style . "github"))
    (:point-up-2-tone4:
     (unicode . "👆🏾")
     (image . "point_up_2_tone4.png")
     (style . "github"))
    (:guardsman_tone1:
     (unicode . "💂🏻")
     (image . "guardsman_tone1.png")
     (style . "github"))
    (:raised-hand-tone3:
     (unicode . "✋🏽")
     (image . "raised_hand_tone3.png")
     (style . "github"))
    (:oil_drum:
     (unicode . "🛢")
     (image . "oil.png")
     (style . "github"))
    (:family-mmgg:
     (unicode . "👨👨👧👧")
     (image . "family_mmgg.png")
     (style . "github"))
    (:muscle:
     (unicode . "💪")
     (image . "muscle.png")
     (style . "github"))
    (:princess_tone2:
     (unicode . "👸🏼")
     (image . "princess_tone2.png")
     (style . "github"))
    (:turkmenistan:
     (unicode . "🇹🇲")
     (image . "flag_tm.png")
     (style . "github"))
    (:family_mmg:
     (unicode . "👨👨👧")
     (image . "family_mmg.png")
     (style . "github"))
    (:family-mwgg:
     (unicode . "👨👩👧👧")
     (image . "family_mwgg.png")
     (style . "github"))
    (:grandma:
     (unicode . "👵")
     (image . "older_woman.png")
     (style . "github"))
    (:flag-th:
     (unicode . "🇹🇭")
     (image . "flag_th.png")
     (style . "github"))
    (:writing-hand-tone3:
     (unicode . "✍🏽")
     (image . "writing_hand_tone3.png")
     (style . "github"))
    (:crying_cat_face:
     (unicode . "😿")
     (image . "crying_cat_face.png")
     (style . "github"))
    (:wastebasket:
     (unicode . "🗑")
     (image . "wastebasket.png")
     (style . "github"))
    (:tokyo-tower:
     (unicode . "🗼")
     (image . "tokyo_tower.png")
     (style . "github"))
    (:smiley-cat:
     (unicode . "😺")
     (image . "smiley_cat.png")
     (style . "github"))
    (:ice_cream:
     (unicode . "🍨")
     (image . "ice_cream.png")
     (style . "github"))
    (:flag-cm:
     (unicode . "🇨🇲")
     (image . "flag_cm.png")
     (style . "github"))
    (:flag_ci:
     (unicode . "🇨🇮")
     (image . "flag_ci.png")
     (style . "github"))
    (:film-frames:
     (unicode . "🎞")
     (image . "film_frames.png")
     (style . "github"))
    (:st:
     (unicode . "🇸🇹")
     (image . "flag_st.png")
     (style . "github"))
    (:flag-ea:
     (unicode . "🇪🇦")
     (image . "flag_ea.png")
     (style . "github"))
    (:flag_af:
     (unicode . "🇦🇫")
     (image . "flag_af.png")
     (style . "github"))
    (:ok_hand_tone2:
     (unicode . "👌🏼")
     (image . "ok_hand_tone2.png")
     (style . "github"))
    (:eye-in-speech-bubble:
     (unicode . "👁🗨")
     (image . "eye_in_speech_bubble.png")
     (style . "github"))
    (:flag-sx:
     (unicode . "🇸🇽")
     (image . "flag_sx.png")
     (style . "github"))
    (:flag-gm:
     (unicode . "🇬🇲")
     (image . "flag_gm.png")
     (style . "github"))
    (:be:
     (unicode . "🇧🇪")
     (image . "flag_be.png")
     (style . "github"))
    (:lk:
     (unicode . "🇱🇰")
     (image . "flag_lk.png")
     (style . "github"))
    (:flag-km:
     (unicode . "🇰🇲")
     (image . "flag_km.png")
     (style . "github"))
    (:pen-fountain:
     (unicode . "🖋")
     (image . "pen_fountain.png")
     (style . "github"))
    (:white_sun_cloud:
     (unicode . "🌥")
     (image . "white_sun_cloud.png")
     (style . "github"))
    (:flag-mq:
     (unicode . "🇲🇶")
     (image . "flag_mq.png")
     (style . "github"))
    (:person-with-blond-hair-tone5:
     (unicode . "👱🏿")
     (image . "person_with_blond_hair_tone5.png")
     (style . "github"))
    (:kiss:
     (unicode . "💋")
     (image . "kiss.png")
     (style . "github"))
    (:runner:
     (unicode . "🏃")
     (image . "runner.png")
     (style . "github"))
    (:paperclips:
     (unicode . "🖇")
     (image . "paperclips.png")
     (style . "github"))
    (:vibration_mode:
     (unicode . "📳")
     (image . "vibration_mode.png")
     (style . "github"))
    (:shirt:
     (unicode . "👕")
     (image . "shirt.png")
     (style . "github"))
    (:grimacing:
     (unicode . "😬")
     (image . "grimacing.png")
     (style . "github"))
    (:v_tone1:
     (unicode . "✌🏻")
     (image . "v_tone1.png")
     (style . "github"))
    (:chocolate-bar:
     (unicode . "🍫")
     (image . "chocolate_bar.png")
     (style . "github"))
    (:man-with-turban-tone1:
     (unicode . "👳🏻")
     (image . "man_with_turban_tone1.png")
     (style . "github"))
    (:person_with_blond_hair:
     (unicode . "👱")
     (image . "person_with_blond_hair.png")
     (style . "github"))
    (:paperclip:
     (unicode . "📎")
     (image . "paperclip.png")
     (style . "github"))
    (:flag_tz:
     (unicode . "🇹🇿")
     (image . "flag_tz.png")
     (style . "github"))
    (:book:
     (unicode . "📖")
     (image . "book.png")
     (style . "github"))
    (:woman_tone2:
     (unicode . "👩🏼")
     (image . "woman_tone2.png")
     (style . "github"))
    (:flag-er:
     (unicode . "🇪🇷")
     (image . "flag_er.png")
     (style . "github"))
    (:trolleybus:
     (unicode . "🚎")
     (image . "trolleybus.png")
     (style . "github"))
    (:angel:
     (unicode . "👼")
     (image . "angel.png")
     (style . "github"))
    (:vertical_traffic_light:
     (unicode . "🚦")
     (image . "vertical_traffic_light.png")
     (style . "github"))
    (:flag_tt:
     (unicode . "🇹🇹")
     (image . "flag_tt.png")
     (style . "github"))
    (:desktop_computer:
     (unicode . "🖥")
     (image . "desktop.png")
     (style . "github"))
    (:heavy_plus_sign:
     (unicode . "➕")
     (image . "heavy_plus_sign.png")
     (style . "github"))
    (:raised_hand_tone5:
     (unicode . "✋🏿")
     (image . "raised_hand_tone5.png")
     (style . "github"))
    (:nail_care_tone1:
     (unicode . "💅🏻")
     (image . "nail_care_tone1.png")
     (style . "github"))
    (:flag-bg:
     (unicode . "🇧🇬")
     (image . "flag_bg.png")
     (style . "github"))
    (:rotating-light:
     (unicode . "🚨")
     (image . "rotating_light.png")
     (style . "github"))
    (:xk:
     (unicode . "🇽🇰")
     (image . "flag_xk.png")
     (style . "github"))
    (:flag-tf:
     (unicode . "🇹🇫")
     (image . "flag_tf.png")
     (style . "github"))
    (:u7a7a:
     (unicode . "🈳")
     (image . "u7a7a.png")
     (style . "github"))
    (:clap-tone3:
     (unicode . "👏🏽")
     (image . "clap_tone3.png")
     (style . "github"))
    (:paintbrush:
     (unicode . "🖌")
     (image . "paintbrush.png")
     (style . "github"))
    (:tj:
     (unicode . "🇹🇯")
     (image . "flag_tj.png")
     (style . "github"))
    (:mv:
     (unicode . "🇲🇻")
     (image . "flag_mv.png")
     (style . "github"))
    (:massage-tone5:
     (unicode . "💆🏿")
     (image . "massage_tone5.png")
     (style . "github"))
    (:baby-chick:
     (unicode . "🐤")
     (image . "baby_chick.png")
     (style . "github"))
    (:ferry:
     (unicode . "⛴")
     (image . "ferry.png")
     (style . "github"))
    (:guardsman:
     (unicode . "💂")
     (image . "guardsman.png")
     (style . "github"))
    (:bear:
     (unicode . "🐻")
     (image . "bear.png")
     (style . "github"))
    (:arrow-right-hook:
     (unicode . "↪")
     (image . "arrow_right_hook.png")
     (style . "github"))
    (:point_up_2_tone4:
     (unicode . "👆🏾")
     (image . "point_up_2_tone4.png")
     (style . "github"))
    (:frowning2:
     (unicode . "☹")
     (image . "frowning2.png")
     (style . "github"))
    (:trophy:
     (unicode . "🏆")
     (image . "trophy.png")
     (style . "github"))
    (:parking:
     (unicode . "🅿")
     (image . "parking.png")
     (style . "github"))
    (:family_mmgg:
     (unicode . "👨👨👧👧")
     (image . "family_mmgg.png")
     (style . "github"))
    (:ab:
     (unicode . "🆎")
     (image . "ab.png")
     (style . "github"))
    (:rowboat_tone4:
     (unicode . "🚣🏾")
     (image . "rowboat_tone4.png")
     (style . "github"))
    (:\'\)
     (unicode . "😂")
     (image . "joy.png")
     (style . "ascii"))
    (:\'\(
     (unicode . "😢")
     (image . "cry.png")
     (style . "ascii"))
    (:monorail:
     (unicode . "🚝")
     (image . "monorail.png")
     (style . "github"))
    (:flag_ie:
     (unicode . "🇮🇪")
     (image . "flag_ie.png")
     (style . "github"))
    (:flag-tt:
     (unicode . "🇹🇹")
     (image . "flag_tt.png")
     (style . "github"))
    (:girl-tone1:
     (unicode . "👧🏻")
     (image . "girl_tone1.png")
     (style . "github"))
    (:point-up-tone1:
     (unicode . "☝🏻")
     (image . "point_up_tone1.png")
     (style . "github"))
    (:banana:
     (unicode . "🍌")
     (image . "banana.png")
     (style . "github"))
    (:open-hands:
     (unicode . "👐")
     (image . "open_hands.png")
     (style . "github"))
    (:flag_mu:
     (unicode . "🇲🇺")
     (image . "flag_mu.png")
     (style . "github"))
    (:no_good_tone5:
     (unicode . "🙅🏿")
     (image . "no_good_tone5.png")
     (style . "github"))
    (:flag_az:
     (unicode . "🇦🇿")
     (image . "flag_az.png")
     (style . "github"))
    (:sh:
     (unicode . "🇸🇭")
     (image . "flag_sh.png")
     (style . "github"))
    (:scales:
     (unicode . "⚖")
     (image . "scales.png")
     (style . "github"))
    (:snake:
     (unicode . "🐍")
     (image . "snake.png")
     (style . "github"))
    (:family-wwgg:
     (unicode . "👩👩👧👧")
     (image . "family_wwgg.png")
     (style . "github"))
    (:v-tone5:
     (unicode . "✌🏿")
     (image . "v_tone5.png")
     (style . "github"))
    (:disappointed_relieved:
     (unicode . "😥")
     (image . "disappointed_relieved.png")
     (style . "github"))
    (:euro:
     (unicode . "💶")
     (image . "euro.png")
     (style . "github"))
    (:clock230:
     (unicode . "🕝")
     (image . "clock230.png")
     (style . "github"))
    (:anchor:
     (unicode . "⚓")
     (image . "anchor.png")
     (style . "github"))
    (:light-rail:
     (unicode . "🚈")
     (image . "light_rail.png")
     (style . "github"))
    (:by:
     (unicode . "🇧🇾")
     (image . "flag_by.png")
     (style . "github"))
    (:cold-sweat:
     (unicode . "😰")
     (image . "cold_sweat.png")
     (style . "github"))
    (:chestnut:
     (unicode . "🌰")
     (image . "chestnut.png")
     (style . "github"))
    (:hand_splayed_tone5:
     (unicode . "🖐🏿")
     (image . "hand_splayed_tone5.png")
     (style . "github"))
    (:clock3:
     (unicode . "🕒")
     (image . "clock3.png")
     (style . "github"))
    (:repeat-one:
     (unicode . "🔂")
     (image . "repeat_one.png")
     (style . "github"))
    (:mk:
     (unicode . "🇲🇰")
     (image . "flag_mk.png")
     (style . "github"))
    (:rose:
     (unicode . "🌹")
     (image . "rose.png")
     (style . "github"))
    (:chocolate_bar:
     (unicode . "🍫")
     (image . "chocolate_bar.png")
     (style . "github"))
    (:couple-mm:
     (unicode . "👨❤👨")
     (image . "couple_mm.png")
     (style . "github"))
    (:baby_bottle:
     (unicode . "🍼")
     (image . "baby_bottle.png")
     (style . "github"))
    (:construction_worker_tone5:
     (unicode . "👷🏿")
     (image . "construction_worker_tone5.png")
     (style . "github"))
    (:tram:
     (unicode . "🚊")
     (image . "tram.png")
     (style . "github"))
    (:barber:
     (unicode . "💈")
     (image . "barber.png")
     (style . "github"))
    (:ear_tone1:
     (unicode . "👂🏻")
     (image . "ear_tone1.png")
     (style . "github"))
    (:middle_finger:
     (unicode . "🖕")
     (image . "middle_finger.png")
     (style . "github"))
    (:swimmer-tone2:
     (unicode . "🏊🏼")
     (image . "swimmer_tone2.png")
     (style . "github"))
    (:atom:
     (unicode . "⚛")
     (image . "atom.png")
     (style . "github"))
    (:bikini:
     (unicode . "👙")
     (image . "bikini.png")
     (style . "github"))
    (:ghost:
     (unicode . "👻")
     (image . "ghost.png")
     (style . "github"))
    (:bookmark_tabs:
     (unicode . "📑")
     (image . "bookmark_tabs.png")
     (style . "github"))
    (:weight_lifter:
     (unicode . "🏋")
     (image . "lifter.png")
     (style . "github"))
    (:cc:
     (unicode . "🇨🇨")
     (image . "flag_cc.png")
     (style . "github"))
    (:flag_sr:
     (unicode . "🇸🇷")
     (image . "flag_sr.png")
     (style . "github"))
    (:white-check-mark:
     (unicode . "✅")
     (image . "white_check_mark.png")
     (style . "github"))
    (:flag-lv:
     (unicode . "🇱🇻")
     (image . "flag_lv.png")
     (style . "github"))
    (:pn:
     (unicode . "🇵🇳")
     (image . "flag_pn.png")
     (style . "github"))
    (:fork_and_knife:
     (unicode . "🍴")
     (image . "fork_and_knife.png")
     (style . "github"))
    (:punch-tone4:
     (unicode . "👊🏾")
     (image . "punch_tone4.png")
     (style . "github"))
    (:skull:
     (unicode . "💀")
     (image . "skull.png")
     (style . "github"))
    (:police-car:
     (unicode . "🚓")
     (image . "police_car.png")
     (style . "github"))
    (:fo:
     (unicode . "🇫🇴")
     (image . "flag_fo.png")
     (style . "github"))
    (:fishing-pole-and-fish:
     (unicode . "🎣")
     (image . "fishing_pole_and_fish.png")
     (style . "github"))
    (:white-sun-small-cloud:
     (unicode . "🌤")
     (image . "white_sun_small_cloud.png")
     (style . "github"))
    (:flag_es:
     (unicode . "🇪🇸")
     (image . "flag_es.png")
     (style . "github"))
    (:burrito:
     (unicode . "🌯")
     (image . "burrito.png")
     (style . "github"))
    (:information_desk_person_tone4:
     (unicode . "💁🏾")
     (image . "information_desk_person_tone4.png")
     (style . "github"))
    (:admission_tickets:
     (unicode . "🎟")
     (image . "tickets.png")
     (style . "github"))
    (:tone2:
     (unicode . "🏼")
     (image . "tone2.png")
     (style . "github"))
    (:money_mouth_face:
     (unicode . "🤑")
     (image . "money_mouth.png")
     (style . "github"))
    (:mn:
     (unicode . "🇲🇳")
     (image . "flag_mn.png")
     (style . "github"))
    (:white-medium-small-square:
     (unicode . "◽")
     (image . "white_medium_small_square.png")
     (style . "github"))
    (:thumbsdown:
     (unicode . "👎")
     (image . "thumbsdown.png")
     (style . "github"))
    (:princess-tone4:
     (unicode . "👸🏾")
     (image . "princess_tone4.png")
     (style . "github"))
    (:flag_ea:
     (unicode . "🇪🇦")
     (image . "flag_ea.png")
     (style . "github"))
    (:flag_fm:
     (unicode . "🇫🇲")
     (image . "flag_fm.png")
     (style . "github"))
    (:flag_sl:
     (unicode . "🇸🇱")
     (image . "flag_sl.png")
     (style . "github"))
    (:flag_km:
     (unicode . "🇰🇲")
     (image . "flag_km.png")
     (style . "github"))
    (:weight_lifter_tone3:
     (unicode . "🏋🏽")
     (image . "lifter_tone3.png")
     (style . "github"))
    (:bike:
     (unicode . "🚲")
     (image . "bike.png")
     (style . "github"))
    (:ear_tone4:
     (unicode . "👂🏾")
     (image . "ear_tone4.png")
     (style . "github"))
    (:+1_tone1:
     (unicode . "👍🏻")
     (image . "thumbsup_tone1.png")
     (style . "github"))
    (:game-die:
     (unicode . "🎲")
     (image . "game_die.png")
     (style . "github"))
    (:100:
     (unicode . "💯")
     (image . "100.png")
     (style . "github"))
    (:flag_mn:
     (unicode . "🇲🇳")
     (image . "flag_mn.png")
     (style . "github"))
    (:bride-with-veil-tone2:
     (unicode . "👰🏼")
     (image . "bride_with_veil_tone2.png")
     (style . "github"))
    (:flag-gp:
     (unicode . "🇬🇵")
     (image . "flag_gp.png")
     (style . "github"))
    (:iq:
     (unicode . "🇮🇶")
     (image . "flag_iq.png")
     (style . "github"))
    (:smile_cat:
     (unicode . "😸")
     (image . "smile_cat.png")
     (style . "github"))
    (:white-medium-square:
     (unicode . "◻")
     (image . "white_medium_square.png")
     (style . "github"))
    (:do_not_litter:
     (unicode . "🚯")
     (image . "do_not_litter.png")
     (style . "github"))
    (:haircut-tone1:
     (unicode . "💇🏻")
     (image . "haircut_tone1.png")
     (style . "github"))
    (:bow-tone2:
     (unicode . "🙇🏼")
     (image . "bow_tone2.png")
     (style . "github"))
    (:runner_tone2:
     (unicode . "🏃🏼")
     (image . "runner_tone2.png")
     (style . "github"))
    (:flag-si:
     (unicode . "🇸🇮")
     (image . "flag_si.png")
     (style . "github"))
    (:frowning:
     (unicode . "😦")
     (image . "frowning.png")
     (style . "github"))
    (:flag-ml:
     (unicode . "🇲🇱")
     (image . "flag_ml.png")
     (style . "github"))
    (:crossed-flags:
     (unicode . "🎌")
     (image . "crossed_flags.png")
     (style . "github"))
    (:file_cabinet:
     (unicode . "🗄")
     (image . "file_cabinet.png")
     (style . "github"))
    (:yellow_heart:
     (unicode . "💛")
     (image . "yellow_heart.png")
     (style . "github"))
    (:innocent:
     (ascii . "O:-)")
     (unicode . "😇")
     (image . "innocent.png")
     (style . "github"))
    (:flag-ht:
     (unicode . "🇭🇹")
     (image . "flag_ht.png")
     (style . "github"))
    (:reversed_hand_with_middle_finger_extended_tone1:
     (unicode . "🖕🏻")
     (image . "middle_finger_tone1.png")
     (style . "github"))
    (:carousel-horse:
     (unicode . "🎠")
     (image . "carousel_horse.png")
     (style . "github"))
    (:nz:
     (unicode . "🇳🇿")
     (image . "flag_nz.png")
     (style . "github"))
    (:japanese-goblin:
     (unicode . "👺")
     (image . "japanese_goblin.png")
     (style . "github"))
    (:notebook-with-decorative-cover:
     (unicode . "📔")
     (image . "notebook_with_decorative_cover.png")
     (style . "github"))
    (:spaghetti:
     (unicode . "🍝")
     (image . "spaghetti.png")
     (style . "github"))
    (:kw:
     (unicode . "🇰🇼")
     (image . "flag_kw.png")
     (style . "github"))
    (:flag-cc:
     (unicode . "🇨🇨")
     (image . "flag_cc.png")
     (style . "github"))
    (:stop_button:
     (unicode . "⏹")
     (image . "stop_button.png")
     (style . "github"))
    (:man-with-gua-pi-mao-tone4:
     (unicode . "👲🏾")
     (image . "man_with_gua_pi_mao_tone4.png")
     (style . "github"))
    (:flag-um:
     (unicode . "🇺🇲")
     (image . "flag_um.png")
     (style . "github"))
    (:card-index:
     (unicode . "📇")
     (image . "card_index.png")
     (style . "github"))
    (:couple_ww:
     (unicode . "👩❤👩")
     (image . "couple_ww.png")
     (style . "github"))
    (:flag_gm:
     (unicode . "🇬🇲")
     (image . "flag_gm.png")
     (style . "github"))
    (:honey_pot:
     (unicode . "🍯")
     (image . "honey_pot.png")
     (style . "github"))
    (:cu:
     (unicode . "🇨🇺")
     (image . "flag_cu.png")
     (style . "github"))
    (:flag-kr:
     (unicode . "🇰🇷")
     (image . "flag_kr.png")
     (style . "github"))
    (:helmet_with_cross:
     (unicode . "⛑")
     (image . "helmet_with_cross.png")
     (style . "github"))
    (:mw:
     (unicode . "🇲🇼")
     (image . "flag_mw.png")
     (style . "github"))
    (:man_with_gua_pi_mao_tone3:
     (unicode . "👲🏽")
     (image . "man_with_gua_pi_mao_tone3.png")
     (style . "github"))
    (:dove_of_peace:
     (unicode . "🕊")
     (image . "dove.png")
     (style . "github"))
    (:flag_hu:
     (unicode . "🇭🇺")
     (image . "flag_hu.png")
     (style . "github"))
    (:family_wwgg:
     (unicode . "👩👩👧👧")
     (image . "family_wwgg.png")
     (style . "github"))
    (:gemini:
     (unicode . "♊")
     (image . "gemini.png")
     (style . "github"))
    (:alien:
     (unicode . "👽")
     (image . "alien.png")
     (style . "github"))
    (:u6708:
     (unicode . "🈷")
     (image . "u6708.png")
     (style . "github"))
    (:aw:
     (unicode . "🇦🇼")
     (image . "flag_aw.png")
     (style . "github"))
    (:gm:
     (unicode . "🇬🇲")
     (image . "flag_gm.png")
     (style . "github"))
    (:flag_lu:
     (unicode . "🇱🇺")
     (image . "flag_lu.png")
     (style . "github"))
    (:moneybag:
     (unicode . "💰")
     (image . "moneybag.png")
     (style . "github"))
    (:open_hands_tone2:
     (unicode . "👐🏼")
     (image . "open_hands_tone2.png")
     (style . "github"))
    (:speaking_head_in_silhouette:
     (unicode . "🗣")
     (image . "speaking_head.png")
     (style . "github"))
    (:cop_tone2:
     (unicode . "👮🏼")
     (image . "cop_tone2.png")
     (style . "github"))
    (:flag_gt:
     (unicode . "🇬🇹")
     (image . "flag_gt.png")
     (style . "github"))
    (:waning-crescent-moon:
     (unicode . "🌘")
     (image . "waning_crescent_moon.png")
     (style . "github"))
    (:eight_pointed_black_star:
     (unicode . "✴")
     (image . "eight_pointed_black_star.png")
     (style . "github"))
    (:flag_yt:
     (unicode . "🇾🇹")
     (image . "flag_yt.png")
     (style . "github"))
    (:tropical_drink:
     (unicode . "🍹")
     (image . "tropical_drink.png")
     (style . "github"))
    (:date:
     (unicode . "📅")
     (image . "date.png")
     (style . "github"))
    (:walking_tone2:
     (unicode . "🚶🏼")
     (image . "walking_tone2.png")
     (style . "github"))
    (:card-box:
     (unicode . "🗃")
     (image . "card_box.png")
     (style . "github"))
    (:bq:
     (unicode . "🇧🇶")
     (image . "flag_bq.png")
     (style . "github"))
    (:arrow_backward:
     (unicode . "◀")
     (image . "arrow_backward.png")
     (style . "github"))
    (:airplane-small:
     (unicode . "🛩")
     (image . "airplane_small.png")
     (style . "github"))
    (:lifter-tone1:
     (unicode . "🏋🏻")
     (image . "lifter_tone1.png")
     (style . "github"))
    (:foggy:
     (unicode . "🌁")
     (image . "foggy.png")
     (style . "github"))
    (:ok_hand_tone3:
     (unicode . "👌🏽")
     (image . "ok_hand_tone3.png")
     (style . "github"))
    (:surfer-tone3:
     (unicode . "🏄🏽")
     (image . "surfer_tone3.png")
     (style . "github"))
    (:flag-cl:
     (unicode . "🇨🇱")
     (image . "flag_cl.png")
     (style . "github"))
    (:flag-ve:
     (unicode . "🇻🇪")
     (image . "flag_ve.png")
     (style . "github"))
    (:cn:
     (unicode . "🇨🇳")
     (image . "flag_cn.png")
     (style . "github"))
    (:hourglass-flowing-sand:
     (unicode . "⏳")
     (image . "hourglass_flowing_sand.png")
     (style . "github"))
    (:mag-right:
     (unicode . "🔎")
     (image . "mag_right.png")
     (style . "github"))
    (:-1:
     (unicode . "👎")
     (image . "thumbsdown.png")
     (style . "github"))
    (:P
     (unicode . "😛")
     (image . "stuck_out_tongue.png")
     (style . "ascii"))
    (:ga:
     (unicode . "🇬🇦")
     (image . "flag_ga.png")
     (style . "github"))
    (:amphora:
     (unicode . "🏺")
     (image . "amphora.png")
     (style . "github"))
    (:man_with_turban_tone4:
     (unicode . "👳🏾")
     (image . "man_with_turban_tone4.png")
     (style . "github"))
    (:walking_tone1:
     (unicode . "🚶🏻")
     (image . "walking_tone1.png")
     (style . "github"))
    (:flag-gl:
     (unicode . "🇬🇱")
     (image . "flag_gl.png")
     (style . "github"))
    (:uy:
     (unicode . "🇺🇾")
     (image . "flag_uy.png")
     (style . "github"))
    (:flag_ps:
     (unicode . "🇵🇸")
     (image . "flag_ps.png")
     (style . "github"))
    (:bd:
     (unicode . "🇧🇩")
     (image . "flag_bd.png")
     (style . "github"))
    (:deciduous_tree:
     (unicode . "🌳")
     (image . "deciduous_tree.png")
     (style . "github"))
    (:cloud_with_rain:
     (unicode . "🌧")
     (image . "cloud_rain.png")
     (style . "github"))
    (:muscle_tone4:
     (unicode . "💪🏾")
     (image . "muscle_tone4.png")
     (style . "github"))
    (:registered:
     (unicode . "®")
     (image . "registered.png")
     (style . "github"))
    (:snowflake:
     (unicode . "❄")
     (image . "snowflake.png")
     (style . "github"))
    (:sg:
     (unicode . "🇸🇬")
     (image . "flag_sg.png")
     (style . "github"))
    (:mag_right:
     (unicode . "🔎")
     (image . "mag_right.png")
     (style . "github"))
    (:christmas-tree:
     (unicode . "🎄")
     (image . "christmas_tree.png")
     (style . "github"))
    (:nr:
     (unicode . "🇳🇷")
     (image . "flag_nr.png")
     (style . "github"))
    (:flag_dg:
     (unicode . "🇩🇬")
     (image . "flag_dg.png")
     (style . "github"))
    (:waning-gibbous-moon:
     (unicode . "🌖")
     (image . "waning_gibbous_moon.png")
     (style . "github"))
    (:beach_umbrella:
     (unicode . "⛱")
     (image . "beach_umbrella.png")
     (style . "github"))
    (:pushpin:
     (unicode . "📌")
     (image . "pushpin.png")
     (style . "github"))
    (:eight_spoked_asterisk:
     (unicode . "✳")
     (image . "eight_spoked_asterisk.png")
     (style . "github"))
    (:sweet_potato:
     (unicode . "🍠")
     (image . "sweet_potato.png")
     (style . "github"))
    (:cop-tone1:
     (unicode . "👮🏻")
     (image . "cop_tone1.png")
     (style . "github"))
    (:man-with-turban-tone2:
     (unicode . "👳🏼")
     (image . "man_with_turban_tone2.png")
     (style . "github"))
    (:neutral-face:
     (unicode . "😐")
     (image . "neutral_face.png")
     (style . "github"))
    (:vulcan-tone3:
     (unicode . "🖖🏽")
     (image . "vulcan_tone3.png")
     (style . "github"))
    (:open-file-folder:
     (unicode . "📂")
     (image . "open_file_folder.png")
     (style . "github"))
    (:flag_bq:
     (unicode . "🇧🇶")
     (image . "flag_bq.png")
     (style . "github"))
    (:cruise_ship:
     (unicode . "🛳")
     (image . "cruise_ship.png")
     (style . "github"))
    (:point_down:
     (unicode . "👇")
     (image . "point_down.png")
     (style . "github"))
    (:negative_squared_cross_mark:
     (unicode . "❎")
     (image . "negative_squared_cross_mark.png")
     (style . "github"))
    (:children-crossing:
     (unicode . "🚸")
     (image . "children_crossing.png")
     (style . "github"))
    (:flag_vi:
     (unicode . "🇻🇮")
     (image . "flag_vi.png")
     (style . "github"))
    (:flag-pw:
     (unicode . "🇵🇼")
     (image . "flag_pw.png")
     (style . "github"))
    (:point-left-tone1:
     (unicode . "👈🏻")
     (image . "point_left_tone1.png")
     (style . "github"))
    (:woman_tone3:
     (unicode . "👩🏽")
     (image . "woman_tone3.png")
     (style . "github"))
    (:bomb:
     (unicode . "💣")
     (image . "bomb.png")
     (style . "github"))
    (:ok-woman-tone5:
     (unicode . "🙆🏿")
     (image . "ok_woman_tone5.png")
     (style . "github"))
    (:orthodox-cross:
     (unicode . "☦")
     (image . "orthodox_cross.png")
     (style . "github"))
    (:speech_balloon:
     (unicode . "💬")
     (image . "speech_balloon.png")
     (style . "github"))
    (:nail_care_tone2:
     (unicode . "💅🏼")
     (image . "nail_care_tone2.png")
     (style . "github"))
    (:hr:
     (unicode . "🇭🇷")
     (image . "flag_hr.png")
     (style . "github"))
    (:helicopter:
     (unicode . "🚁")
     (image . "helicopter.png")
     (style . "github"))
    (:flag-tg:
     (unicode . "🇹🇬")
     (image . "flag_tg.png")
     (style . "github"))
    (:tm:
     (unicode . "™")
     (image . "tm.png")
     (style . "github"))
    (:arrow_double_down:
     (unicode . "⏬")
     (image . "arrow_double_down.png")
     (style . "github"))
    (:kn:
     (unicode . "🇰🇳")
     (image . "flag_kn.png")
     (style . "github"))
    (:massage-tone4:
     (unicode . "💆🏾")
     (image . "massage_tone4.png")
     (style . "github"))
    (:lion-face:
     (unicode . "🦁")
     (image . "lion_face.png")
     (style . "github"))
    (:flag_cc:
     (unicode . "🇨🇨")
     (image . "flag_cc.png")
     (style . "github"))
    (:man-tone4:
     (unicode . "👨🏾")
     (image . "man_tone4.png")
     (style . "github"))
    (:va:
     (unicode . "🇻🇦")
     (image . "flag_va.png")
     (style . "github"))
    (:track-previous:
     (unicode . "⏮")
     (image . "track_previous.png")
     (style . "github"))
    (:pray-tone4:
     (unicode . "🙏🏾")
     (image . "pray_tone4.png")
     (style . "github"))
    (:flame:
     (unicode . "🔥")
     (image . "fire.png")
     (style . "github"))
    (:ok_woman_tone1:
     (unicode . "🙆🏻")
     (image . "ok_woman_tone1.png")
     (style . "github"))
    (:blossom:
     (unicode . "🌼")
     (image . "blossom.png")
     (style . "github"))
    (:bicyclist-tone5:
     (unicode . "🚴🏿")
     (image . "bicyclist_tone5.png")
     (style . "github"))
    (:baby-bottle:
     (unicode . "🍼")
     (image . "baby_bottle.png")
     (style . "github"))
    (:rowboat_tone5:
     (unicode . "🚣🏿")
     (image . "rowboat_tone5.png")
     (style . "github"))
    (:angel-tone1:
     (unicode . "👼🏻")
     (image . "angel_tone1.png")
     (style . "github"))
    (:flag_id:
     (unicode . "🇮🇩")
     (image . "flag_id.png")
     (style . "github"))
    (:girl-tone2:
     (unicode . "👧🏼")
     (image . "girl_tone2.png")
     (style . "github"))
    (:end:
     (unicode . "🔚")
     (image . "end.png")
     (style . "github"))
    (:flag_cr:
     (unicode . "🇨🇷")
     (image . "flag_cr.png")
     (style . "github"))
    (:calendar:
     (unicode . "📆")
     (image . "calendar.png")
     (style . "github"))
    (:flag-mc:
     (unicode . "🇲🇨")
     (image . "flag_mc.png")
     (style . "github"))
    (:flag_eh:
     (unicode . "🇪🇭")
     (image . "flag_eh.png")
     (style . "github"))
    (:baby_tone1:
     (unicode . "👶🏻")
     (image . "baby_tone1.png")
     (style . "github"))
    (:flag_mt:
     (unicode . "🇲🇹")
     (image . "flag_mt.png")
     (style . "github"))
    (:-1_tone5:
     (unicode . "👎🏿")
     (image . "thumbsdown_tone5.png")
     (style . "github"))
    (:no_good_tone4:
     (unicode . "🙅🏾")
     (image . "no_good_tone4.png")
     (style . "github"))
    (:flag_pe:
     (unicode . "🇵🇪")
     (image . "flag_pe.png")
     (style . "github"))
    (:si:
     (unicode . "🇸🇮")
     (image . "flag_si.png")
     (style . "github"))
    (:rice_ball:
     (unicode . "🍙")
     (image . "rice_ball.png")
     (style . "github"))
    (:swimmer-tone5:
     (unicode . "🏊🏿")
     (image . "swimmer_tone5.png")
     (style . "github"))
    (:middle-finger-tone1:
     (unicode . "🖕🏻")
     (image . "middle_finger_tone1.png")
     (style . "github"))
    (:basketball-player-tone1:
     (unicode . "⛹🏻")
     (image . "basketball_player_tone1.png")
     (style . "github"))
    (:japanese_ogre:
     (unicode . "👹")
     (image . "japanese_ogre.png")
     (style . "github"))
    (:house:
     (unicode . "🏠")
     (image . "house.png")
     (style . "github"))
    (:heart_decoration:
     (unicode . "💟")
     (image . "heart_decoration.png")
     (style . "github"))
    (:flag-lk:
     (unicode . "🇱🇰")
     (image . "flag_lk.png")
     (style . "github"))
    (:stadium:
     (unicode . "🏟")
     (image . "stadium.png")
     (style . "github"))
    (:satellite_orbital:
     (unicode . "🛰")
     (image . "satellite_orbital.png")
     (style . "github"))
    (:keyboard:
     (unicode . "⌨")
     (image . "keyboard.png")
     (style . "github"))
    (:girl_tone4:
     (unicode . "👧🏾")
     (image . "girl_tone4.png")
     (style . "github"))
    (:person_with_pouting_face_tone2:
     (unicode . "🙎🏼")
     (image . "person_with_pouting_face_tone2.png")
     (style . "github"))
    (:cp:
     (unicode . "🇨🇵")
     (image . "flag_cp.png")
     (style . "github"))
    (:dango:
     (unicode . "🍡")
     (image . "dango.png")
     (style . "github"))
    (:left_luggage:
     (unicode . "🛅")
     (image . "left_luggage.png")
     (style . "github"))
    (:mh:
     (unicode . "🇲🇭")
     (image . "flag_mh.png")
     (style . "github"))
    (:performing_arts:
     (unicode . "🎭")
     (image . "performing_arts.png")
     (style . "github"))
    (:reversed_hand_with_middle_finger_extended:
     (unicode . "🖕")
     (image . "middle_finger.png")
     (style . "github"))
    (:koko:
     (unicode . "🈁")
     (image . "koko.png")
     (style . "github"))
    (:hankey:
     (unicode . "💩")
     (image . "poop.png")
     (style . "github"))
    (:swimmer-tone3:
     (unicode . "🏊🏽")
     (image . "swimmer_tone3.png")
     (style . "github"))
    (:cookie:
     (unicode . "🍪")
     (image . "cookie.png")
     (style . "github"))
    (:flag-pe:
     (unicode . "🇵🇪")
     (image . "flag_pe.png")
     (style . "github"))
    (:pouch:
     (unicode . "👝")
     (image . "pouch.png")
     (style . "github"))
    (:u7121:
     (unicode . "🈚")
     (image . "u7121.png")
     (style . "github"))
    (:construction-worker:
     (unicode . "👷")
     (image . "construction_worker.png")
     (style . "github"))
    (:ferris_wheel:
     (unicode . "🎡")
     (image . "ferris_wheel.png")
     (style . "github"))
    (:flag_er:
     (unicode . "🇪🇷")
     (image . "flag_er.png")
     (style . "github"))
    (:eh:
     (unicode . "🇪🇭")
     (image . "flag_eh.png")
     (style . "github"))
    (:punch-tone5:
     (unicode . "👊🏿")
     (image . "punch_tone5.png")
     (style . "github"))
    (:european-post-office:
     (unicode . "🏤")
     (image . "european_post_office.png")
     (style . "github"))
    (:tiger:
     (unicode . "🐯")
     (image . "tiger.png")
     (style . "github"))
    (:envelope_with_arrow:
     (unicode . "📩")
     (image . "envelope_with_arrow.png")
     (style . "github"))
    (:four-leaf-clover:
     (unicode . "🍀")
     (image . "four_leaf_clover.png")
     (style . "github"))
    (:flag-ng:
     (unicode . "🇳🇬")
     (image . "flag_ng.png")
     (style . "github"))
    (:dromedary-camel:
     (unicode . "🐪")
     (image . "dromedary_camel.png")
     (style . "github"))
    (:clap-tone4:
     (unicode . "👏🏾")
     (image . "clap_tone4.png")
     (style . "github"))
    (:older_man_tone1:
     (unicode . "👴🏻")
     (image . "older_man_tone1.png")
     (style . "github"))
    (:heart_eyes:
     (unicode . "😍")
     (image . "heart_eyes.png")
     (style . "github"))
    (:mountain-bicyclist:
     (unicode . "🚵")
     (image . "mountain_bicyclist.png")
     (style . "github"))
    (:hn:
     (unicode . "🇭🇳")
     (image . "flag_hn.png")
     (style . "github"))
    (:floppy-disk:
     (unicode . "💾")
     (image . "floppy_disk.png")
     (style . "github"))
    (:abcd:
     (unicode . "🔡")
     (image . "abcd.png")
     (style . "github"))
    (:man-with-gua-pi-mao:
     (unicode . "👲")
     (image . "man_with_gua_pi_mao.png")
     (style . "github"))
    (:black_joker:
     (unicode . "🃏")
     (image . "black_joker.png")
     (style . "github"))
    (:coffin:
     (unicode . "⚰")
     (image . "coffin.png")
     (style . "github"))
    (:levitate:
     (unicode . "🕴")
     (image . "levitate.png")
     (style . "github"))
    (:flag_lt:
     (unicode . "🇱🇹")
     (image . "flag_lt.png")
     (style . "github"))
    (:toilet:
     (unicode . "🚽")
     (image . "toilet.png")
     (style . "github"))
    (:flag_fj:
     (unicode . "🇫🇯")
     (image . "flag_fj.png")
     (style . "github"))
    (:weight_lifter_tone2:
     (unicode . "🏋🏼")
     (image . "lifter_tone2.png")
     (style . "github"))
    (:radio_button:
     (unicode . "🔘")
     (image . "radio_button.png")
     (style . "github"))
    (:spy-tone1:
     (unicode . "🕵🏻")
     (image . "spy_tone1.png")
     (style . "github"))
    (:dove:
     (unicode . "🕊")
     (image . "dove.png")
     (style . "github"))
    (:+1_tone2:
     (unicode . "👍🏼")
     (image . "thumbsup_tone2.png")
     (style . "github"))
    (:koala:
     (unicode . "🐨")
     (image . "koala.png")
     (style . "github"))
    (:pw:
     (unicode . "🇵🇼")
     (image . "flag_pw.png")
     (style . "github"))
    (:baseball:
     (unicode . "⚾")
     (image . "baseball.png")
     (style . "github"))
    (:bride-with-veil-tone3:
     (unicode . "👰🏽")
     (image . "bride_with_veil_tone3.png")
     (style . "github"))
    (:oncoming-automobile:
     (unicode . "🚘")
     (image . "oncoming_automobile.png")
     (style . "github"))
    (:postbox:
     (unicode . "📮")
     (image . "postbox.png")
     (style . "github"))
    (:oncoming-bus:
     (unicode . "🚍")
     (image . "oncoming_bus.png")
     (style . "github"))
    (:postal-horn:
     (unicode . "📯")
     (image . "postal_horn.png")
     (style . "github"))
    (:flag-by:
     (unicode . "🇧🇾")
     (image . "flag_by.png")
     (style . "github"))
    (:gp:
     (unicode . "🇬🇵")
     (image . "flag_gp.png")
     (style . "github"))
    (:flag_jp:
     (unicode . "🇯🇵")
     (image . "flag_jp.png")
     (style . "github"))
    (:children_crossing:
     (unicode . "🚸")
     (image . "children_crossing.png")
     (style . "github"))
    (:star-of-david:
     (unicode . "✡")
     (image . "star_of_david.png")
     (style . "github"))
    (:runner_tone3:
     (unicode . "🏃🏽")
     (image . "runner_tone3.png")
     (style . "github"))
    (:flag-mo:
     (unicode . "🇲🇴")
     (image . "flag_mo.png")
     (style . "github"))
    (:mailbox-closed:
     (unicode . "📪")
     (image . "mailbox_closed.png")
     (style . "github"))
    (:helmet_with_white_cross:
     (unicode . "⛑")
     (image . "helmet_with_cross.png")
     (style . "github"))
    (:id:
     (unicode . "🆔")
     (image . "id.png")
     (style . "github"))
    (:flag-hu:
     (unicode . "🇭🇺")
     (image . "flag_hu.png")
     (style . "github"))
    (:point-left-tone3:
     (unicode . "👈🏽")
     (image . "point_left_tone3.png")
     (style . "github"))
    (:tired_face:
     (unicode . "😫")
     (image . "tired_face.png")
     (style . "github"))
    (:see-no-evil:
     (unicode . "🙈")
     (image . "see_no_evil.png")
     (style . "github"))
    (:af:
     (unicode . "🇦🇫")
     (image . "flag_af.png")
     (style . "github"))
    (:v-tone1:
     (unicode . "✌🏻")
     (image . "v_tone1.png")
     (style . "github"))
    (:large_blue_circle:
     (unicode . "🔵")
     (image . "large_blue_circle.png")
     (style . "github"))
    (:school:
     (unicode . "🏫")
     (image . "school.png")
     (style . "github"))
    (:handbag:
     (unicode . "👜")
     (image . "handbag.png")
     (style . "github"))
    (:cv:
     (unicode . "🇨🇻")
     (image . "flag_cv.png")
     (style . "github"))
    (:flag_so:
     (unicode . "🇸🇴")
     (image . "flag_so.png")
     (style . "github"))
    (:stuck_out_tongue_winking_eye:
     (ascii . ">:P")
     (unicode . "😜")
     (image . "stuck_out_tongue_winking_eye.png")
     (style . "github"))
    (:mt:
     (unicode . "🇲🇹")
     (image . "flag_mt.png")
     (style . "github"))
    (:horse-racing-tone1:
     (unicode . "🏇🏻")
     (image . "horse_racing_tone1.png")
     (style . "github"))
    (:metro:
     (unicode . "🚇")
     (image . "metro.png")
     (style . "github"))
    (:older_woman_tone3:
     (unicode . "👵🏽")
     (image . "older_woman_tone3.png")
     (style . "github"))
    (:flag_gl:
     (unicode . "🇬🇱")
     (image . "flag_gl.png")
     (style . "github"))
    (:astonished:
     (unicode . "😲")
     (image . "astonished.png")
     (style . "github"))
    (:flag_dj:
     (unicode . "🇩🇯")
     (image . "flag_dj.png")
     (style . "github"))
    (:four:
     (unicode . "4⃣")
     (image . "four.png")
     (style . "github"))
    (:high_heel:
     (unicode . "👠")
     (image . "high_heel.png")
     (style . "github"))
    (:Þ
     (unicode . "😛")
     (image . "stuck_out_tongue.png")
     (style . "ascii"))
    (:flag-hk:
     (unicode . "🇭🇰")
     (image . "flag_hk.png")
     (style . "github"))
    (:flag_tj:
     (unicode . "🇹🇯")
     (image . "flag_tj.png")
     (style . "github"))
    (:open_hands_tone1:
     (unicode . "👐🏻")
     (image . "open_hands_tone1.png")
     (style . "github"))
    (:bride_with_veil_tone4:
     (unicode . "👰🏾")
     (image . "bride_with_veil_tone4.png")
     (style . "github"))
    (:four_leaf_clover:
     (unicode . "🍀")
     (image . "four_leaf_clover.png")
     (style . "github"))
    (:sleuth_or_spy:
     (unicode . "🕵")
     (image . "spy.png")
     (style . "github"))
    (:arrow-heading-down:
     (unicode . "⤵")
     (image . "arrow_heading_down.png")
     (style . "github"))
    (:cw:
     (unicode . "🇨🇼")
     (image . "flag_cw.png")
     (style . "github"))
    (:arrow-lower-right:
     (unicode . "↘")
     (image . "arrow_lower_right.png")
     (style . "github"))
    (:izakaya_lantern:
     (unicode . "🏮")
     (image . "izakaya_lantern.png")
     (style . "github"))
    (:rolled_up_newspaper:
     (unicode . "🗞")
     (image . "newspaper2.png")
     (style . "github"))
    (:point-up-2-tone2:
     (unicode . "👆🏼")
     (image . "point_up_2_tone2.png")
     (style . "github"))
    (:bg:
     (unicode . "🇧🇬")
     (image . "flag_bg.png")
     (style . "github"))
    (:rugby_football:
     (unicode . "🏉")
     (image . "rugby_football.png")
     (style . "github"))
    (:green-heart:
     (unicode . "💚")
     (image . "green_heart.png")
     (style . "github"))
    (:mobile_phone_off:
     (unicode . "📴")
     (image . "mobile_phone_off.png")
     (style . "github"))
    (:flag_li:
     (unicode . "🇱🇮")
     (image . "flag_li.png")
     (style . "github"))
    (:hand-splayed-tone1:
     (unicode . "🖐🏻")
     (image . "hand_splayed_tone1.png")
     (style . "github"))
    (:mu:
     (unicode . "🇲🇺")
     (image . "flag_mu.png")
     (style . "github"))
    (:chart-with-upwards-trend:
     (unicode . "📈")
     (image . "chart_with_upwards_trend.png")
     (style . "github"))
    (:copyright:
     (unicode . "©")
     (image . "copyright.png")
     (style . "github"))
    (:traffic_light:
     (unicode . "🚥")
     (image . "traffic_light.png")
     (style . "github"))
    (:li:
     (unicode . "🇱🇮")
     (image . "flag_li.png")
     (style . "github"))
    (:az:
     (unicode . "🇦🇿")
     (image . "flag_az.png")
     (style . "github"))
    (:surfer-tone2:
     (unicode . "🏄🏼")
     (image . "surfer_tone2.png")
     (style . "github"))
    (:lifter:
     (unicode . "🏋")
     (image . "lifter.png")
     (style . "github"))
    (:flag-ma:
     (unicode . "🇲🇦")
     (image . "flag_ma.png")
     (style . "github"))
    (:sv:
     (unicode . "🇸🇻")
     (image . "flag_sv.png")
     (style . "github"))
    (:raising_hand_tone3:
     (unicode . "🙋🏽")
     (image . "raising_hand_tone3.png")
     (style . "github"))
    (:small-red-triangle-down:
     (unicode . "🔻")
     (image . "small_red_triangle_down.png")
     (style . "github"))
    (:vulcan_tone3:
     (unicode . "🖖🏽")
     (image . "vulcan_tone3.png")
     (style . "github"))
    (:flag_ad:
     (unicode . "🇦🇩")
     (image . "flag_ad.png")
     (style . "github"))
    (:ok-woman-tone3:
     (unicode . "🙆🏽")
     (image . "ok_woman_tone3.png")
     (style . "github"))
    (:man_with_turban_tone5:
     (unicode . "👳🏿")
     (image . "man_with_turban_tone5.png")
     (style . "github"))
    (:point_left_tone2:
     (unicode . "👈🏼")
     (image . "point_left_tone2.png")
     (style . "github"))
    (:hammer:
     (unicode . "🔨")
     (image . "hammer.png")
     (style . "github"))
    (:tokyo_tower:
     (unicode . "🗼")
     (image . "tokyo_tower.png")
     (style . "github"))
    (:maple-leaf:
     (unicode . "🍁")
     (image . "maple_leaf.png")
     (style . "github"))
    (:jack_o_lantern:
     (unicode . "🎃")
     (image . "jack_o_lantern.png")
     (style . "github"))
    (:flag_pt:
     (unicode . "🇵🇹")
     (image . "flag_pt.png")
     (style . "github"))
    (:D
     (unicode . "😃")
     (image . "smiley.png")
     (style . "ascii"))
    (:\[
     (unicode . "😞")
     (image . "disappointed.png")
     (style . "ascii"))
    (:goat:
     (unicode . "🐐")
     (image . "goat.png")
     (style . "github"))
    (:X
     (unicode . "😶")
     (image . "no_mouth.png")
     (style . "ascii"))
    (:sx:
     (unicode . "🇸🇽")
     (image . "flag_sx.png")
     (style . "github"))
    (:\]
     (unicode . "😄")
     (image . "smile.png")
     (style . "ascii"))
    (:\\
     (unicode . "😕")
     (image . "confused.png")
     (style . "ascii"))
    (:dolls:
     (unicode . "🎎")
     (image . "dolls.png")
     (style . "github"))
    (:@
     (unicode . "😠")
     (image . "angry.png")
     (style . "ascii"))
    (:muscle-tone4:
     (unicode . "💪🏾")
     (image . "muscle_tone4.png")
     (style . "github"))
    (:flag-ms:
     (unicode . "🇲🇸")
     (image . "flag_ms.png")
     (style . "github"))
    (:O
     (unicode . "😮")
     (image . "open_mouth.png")
     (style . "ascii"))
    (:face_with_thermometer:
     (unicode . "🤒")
     (image . "thermometer_face.png")
     (style . "github"))
    (:L
     (unicode . "😕")
     (image . "confused.png")
     (style . "ascii"))
    (:ledger:
     (unicode . "📒")
     (image . "ledger.png")
     (style . "github"))
    (:flag_td:
     (unicode . "🇹🇩")
     (image . "flag_td.png")
     (style . "github"))
    (:p
     (unicode . "😛")
     (image . "stuck_out_tongue.png")
     (style . "ascii"))
    (:man-with-turban-tone3:
     (unicode . "👳🏽")
     (image . "man_with_turban_tone3.png")
     (style . "github"))
    (:flag_cu:
     (unicode . "🇨🇺")
     (image . "flag_cu.png")
     (style . "github"))
    (:x
     (unicode . "😶")
     (image . "no_mouth.png")
     (style . "ascii"))
    (:factory:
     (unicode . "🏭")
     (image . "factory.png")
     (style . "github"))
    (:b
     (unicode . "😛")
     (image . "stuck_out_tongue.png")
     (style . "ascii"))
    (:open_hands:
     (unicode . "👐")
     (image . "open_hands.png")
     (style . "github"))
    (:lifter-tone2:
     (unicode . "🏋🏼")
     (image . "lifter_tone2.png")
     (style . "github"))
    (:part_alternation_mark:
     (unicode . "〽")
     (image . "part_alternation_mark.png")
     (style . "github"))
    (:nu:
     (unicode . "🇳🇺")
     (image . "flag_nu.png")
     (style . "github"))
    (:name_badge:
     (unicode . "📛")
     (image . "name_badge.png")
     (style . "github"))
    (:o
     (unicode . "😮")
     (image . "open_mouth.png")
     (style . "ascii"))
    (:flag_bv:
     (unicode . "🇧🇻")
     (image . "flag_bv.png")
     (style . "github"))
    (:rolling-eyes:
     (unicode . "🙄")
     (image . "rolling_eyes.png")
     (style . "github"))
    (:flag-mm:
     (unicode . "🇲🇲")
     (image . "flag_mm.png")
     (style . "github"))
    (:v_tone3:
     (unicode . "✌🏽")
     (image . "v_tone3.png")
     (style . "github"))
    (:sandal:
     (unicode . "👡")
     (image . "sandal.png")
     (style . "github"))
    (:vulcan-tone5:
     (unicode . "🖖🏿")
     (image . "vulcan_tone5.png")
     (style . "github"))
    (:construction-worker-tone2:
     (unicode . "👷🏼")
     (image . "construction_worker_tone2.png")
     (style . "github"))
    (:flag-pt:
     (unicode . "🇵🇹")
     (image . "flag_pt.png")
     (style . "github"))
    (:ok:
     (unicode . "🆗")
     (image . "ok.png")
     (style . "github"))
    (:flag_tv:
     (unicode . "🇹🇻")
     (image . "flag_tv.png")
     (style . "github"))
    (:nail_care_tone3:
     (unicode . "💅🏽")
     (image . "nail_care_tone3.png")
     (style . "github"))
    (:santa-tone5:
     (unicode . "🎅🏿")
     (image . "santa_tone5.png")
     (style . "github"))
    (:telephone-receiver:
     (unicode . "📞")
     (image . "telephone_receiver.png")
     (style . "github"))
    (:kissing:
     (unicode . "😗")
     (image . "kissing.png")
     (style . "github"))
    (:peach:
     (unicode . "🍑")
     (image . "peach.png")
     (style . "github"))
    (:iphone:
     (unicode . "📱")
     (image . "iphone.png")
     (style . "github"))
    (:flag_bm:
     (unicode . "🇧🇲")
     (image . "flag_bm.png")
     (style . "github"))
    (:tl:
     (unicode . "🇹🇱")
     (image . "flag_tl.png")
     (style . "github"))
    (:bicyclist_tone3:
     (unicode . "🚴🏽")
     (image . "bicyclist_tone3.png")
     (style . "github"))
    (:helmet-with-cross:
     (unicode . "⛑")
     (image . "helmet_with_cross.png")
     (style . "github"))
    (:flag-td:
     (unicode . "🇹🇩")
     (image . "flag_td.png")
     (style . "github"))
    (:\#
     (unicode . "😶")
     (image . "no_mouth.png")
     (style . "ascii"))
    (:confounded:
     (unicode . "😖")
     (image . "confounded.png")
     (style . "github"))
    (:$
     (unicode . "😳")
     (image . "flushed.png")
     (style . "ascii"))
    (:basketball-player-tone2:
     (unicode . "⛹🏼")
     (image . "basketball_player_tone2.png")
     (style . "github"))
    (:*
     (unicode . "😘")
     (image . "kissing_heart.png")
     (style . "ascii"))
    (:\)
     (unicode . "😄")
     (image . "smile.png")
     (style . "ascii"))
    (:flag-ca:
     (unicode . "🇨🇦")
     (image . "flag_ca.png")
     (style . "github"))
    (:/
     (unicode . "😕")
     (image . "confused.png")
     (style . "ascii"))
    (:basketball_player:
     (unicode . "⛹")
     (image . "basketball_player.png")
     (style . "github"))
    (:pray-tone5:
     (unicode . "🙏🏿")
     (image . "pray_tone5.png")
     (style . "github"))
    (:fork-and-knife:
     (unicode . "🍴")
     (image . "fork_and_knife.png")
     (style . "github"))
    (:ice-skate:
     (unicode . "⛸")
     (image . "ice_skate.png")
     (style . "github"))
    (:calendar-spiral:
     (unicode . "🗓")
     (image . "calendar_spiral.png")
     (style . "github"))
    (:rowboat_tone2:
     (unicode . "🚣🏼")
     (image . "rowboat_tone2.png")
     (style . "github"))
    (:cow:
     (unicode . "🐮")
     (image . "cow.png")
     (style . "github"))
    (:smiley_cat:
     (unicode . "😺")
     (image . "smiley_cat.png")
     (style . "github"))
    (:cold_sweat:
     (unicode . "😰")
     (image . "cold_sweat.png")
     (style . "github"))
    (:flag-ie:
     (unicode . "🇮🇪")
     (image . "flag_ie.png")
     (style . "github"))
    (:doughnut:
     (unicode . "🍩")
     (image . "doughnut.png")
     (style . "github"))
    (:point-down-tone2:
     (unicode . "👇🏼")
     (image . "point_down_tone2.png")
     (style . "github"))
    (:girl-tone3:
     (unicode . "👧🏽")
     (image . "girl_tone3.png")
     (style . "github"))
    (:family-wwb:
     (unicode . "👩👩👦")
     (image . "family_wwb.png")
     (style . "github"))
    (:flag-tr:
     (unicode . "🇹🇷")
     (image . "flag_tr.png")
     (style . "github"))
    (:three_button_mouse:
     (unicode . "🖱")
     (image . "mouse_three_button.png")
     (style . "github"))
    (:baby_tone3:
     (unicode . "👶🏽")
     (image . "baby_tone3.png")
     (style . "github"))
    (:bi:
     (unicode . "🇧🇮")
     (image . "flag_bi.png")
     (style . "github"))
    (:metal-tone5:
     (unicode . "🤘🏿")
     (image . "metal_tone5.png")
     (style . "github"))
    (:flag_br:
     (unicode . "🇧🇷")
     (image . "flag_br.png")
     (style . "github"))
    (:no_good_tone3:
     (unicode . "🙅🏽")
     (image . "no_good_tone3.png")
     (style . "github"))
    (:flag_ms:
     (unicode . "🇲🇸")
     (image . "flag_ms.png")
     (style . "github"))
    (:sj:
     (unicode . "🇸🇯")
     (image . "flag_sj.png")
     (style . "github"))
    (:track-next:
     (unicode . "⏭")
     (image . "track_next.png")
     (style . "github"))
    (:partly-sunny:
     (unicode . "⛅")
     (image . "partly_sunny.png")
     (style . "github"))
    (:flag_pf:
     (unicode . "🇵🇫")
     (image . "flag_pf.png")
     (style . "github"))
    (:basketball_player_tone1:
     (unicode . "⛹🏻")
     (image . "basketball_player_tone1.png")
     (style . "github"))
    (:japanese-ogre:
     (unicode . "👹")
     (image . "japanese_ogre.png")
     (style . "github"))
    (:flag_ph:
     (unicode . "🇵🇭")
     (image . "flag_ph.png")
     (style . "github"))
    (:new-moon-with-face:
     (unicode . "🌚")
     (image . "new_moon_with_face.png")
     (style . "github"))
    (:arrow_down_small:
     (unicode . "🔽")
     (image . "arrow_down_small.png")
     (style . "github"))
    (:flag_ki:
     (unicode . "🇰🇮")
     (image . "flag_ki.png")
     (style . "github"))
    (:snowman:
     (unicode . "⛄")
     (image . "snowman.png")
     (style . "github"))
    (:expressionless:
     (ascii . "-_-")
     (unicode . "😑")
     (image . "expressionless.png")
     (style . "github"))
    (:musical-keyboard:
     (unicode . "🎹")
     (image . "musical_keyboard.png")
     (style . "github"))
    (:round-pushpin:
     (unicode . "📍")
     (image . "round_pushpin.png")
     (style . "github"))
    (:nose_tone1:
     (unicode . "👃🏻")
     (image . "nose_tone1.png")
     (style . "github"))
    (:inbox_tray:
     (unicode . "📥")
     (image . "inbox_tray.png")
     (style . "github"))
    (:mouse2:
     (unicode . "🐁")
     (image . "mouse2.png")
     (style . "github"))
    (:fire:
     (unicode . "🔥")
     (image . "fire.png")
     (style . "github"))
    (:hatched_chick:
     (unicode . "🐥")
     (image . "hatched_chick.png")
     (style . "github"))
    (:simple_smile:
     (ascii . ":)")
     (unicode . "🙂")
     (image . "smile.png")
     (style . "github"))
    (:arrows-counterclockwise:
     (unicode . "🔄")
     (image . "arrows_counterclockwise.png")
     (style . "github"))
    (:sweat_drops:
     (unicode . "💦")
     (image . "sweat_drops.png")
     (style . "github"))
    (:baggage-claim:
     (unicode . "🛄")
     (image . "baggage_claim.png")
     (style . "github"))
    (:do:
     (unicode . "🇩🇴")
     (image . "flag_do.png")
     (style . "github"))
    (:om-symbol:
     (unicode . "🕉")
     (image . "om_symbol.png")
     (style . "github"))
    (:open-hands-tone5:
     (unicode . "👐🏿")
     (image . "open_hands_tone5.png")
     (style . "github"))
    (:flag-lt:
     (unicode . "🇱🇹")
     (image . "flag_lt.png")
     (style . "github"))
    (:family_wwb:
     (unicode . "👩👩👦")
     (image . "family_wwb.png")
     (style . "github"))
    (:nerd_face:
     (unicode . "🤓")
     (image . "nerd.png")
     (style . "github"))
    (:bow-and-arrow:
     (unicode . "🏹")
     (image . "bow_and_arrow.png")
     (style . "github"))
    (:point_down_tone1:
     (unicode . "👇🏻")
     (image . "point_down_tone1.png")
     (style . "github"))
    (:linked_paperclips:
     (unicode . "🖇")
     (image . "paperclips.png")
     (style . "github"))
    (:checkered-flag:
     (unicode . "🏁")
     (image . "checkered_flag.png")
     (style . "github"))
    (:poo:
     (unicode . "💩")
     (image . "poop.png")
     (style . "github"))
    (:flag_lk:
     (unicode . "🇱🇰")
     (image . "flag_lk.png")
     (style . "github"))
    (:ph:
     (unicode . "🇵🇭")
     (image . "flag_ph.png")
     (style . "github"))
    (:punch-tone2:
     (unicode . "👊🏼")
     (image . "punch_tone2.png")
     (style . "github"))
    (:flag-ph:
     (unicode . "🇵🇭")
     (image . "flag_ph.png")
     (style . "github"))
    (:hear-no-evil:
     (unicode . "🙉")
     (image . "hear_no_evil.png")
     (style . "github"))
    (:fm:
     (unicode . "🇫🇲")
     (image . "flag_fm.png")
     (style . "github"))
    (:person-with-pouting-face-tone1:
     (unicode . "🙎🏻")
     (image . "person_with_pouting_face_tone1.png")
     (style . "github"))
    (:shamrock:
     (unicode . "☘")
     (image . "shamrock.png")
     (style . "github"))
    (:information_desk_person_tone2:
     (unicode . "💁🏼")
     (image . "information_desk_person_tone2.png")
     (style . "github"))
    (:tiger2:
     (unicode . "🐅")
     (image . "tiger2.png")
     (style . "github"))
    (:arrow_upper_left:
     (unicode . "↖")
     (image . "arrow_upper_left.png")
     (style . "github"))
    (:point-right:
     (unicode . "👉")
     (image . "point_right.png")
     (style . "github"))
    (:point-up-2-tone1:
     (unicode . "👆🏻")
     (image . "point_up_2_tone1.png")
     (style . "github"))
    (:house-with-garden:
     (unicode . "🏡")
     (image . "house_with_garden.png")
     (style . "github"))
    (:spy-tone2:
     (unicode . "🕵🏼")
     (image . "spy_tone2.png")
     (style . "github"))
    (:flag-vu:
     (unicode . "🇻🇺")
     (image . "flag_vu.png")
     (style . "github"))
    (:fish-cake:
     (unicode . "🍥")
     (image . "fish_cake.png")
     (style . "github"))
    (:fog:
     (unicode . "🌫")
     (image . "fog.png")
     (style . "github"))
    (:grapes:
     (unicode . "🍇")
     (image . "grapes.png")
     (style . "github"))
    (:statue-of-liberty:
     (unicode . "🗽")
     (image . "statue_of_liberty.png")
     (style . "github"))
    (:back:
     (unicode . "🔙")
     (image . "back.png")
     (style . "github"))
    (:strawberry:
     (unicode . "🍓")
     (image . "strawberry.png")
     (style . "github"))
    (:recycle:
     (unicode . "♻")
     (image . "recycle.png")
     (style . "github"))
    (:ice-cream:
     (unicode . "🍨")
     (image . "ice_cream.png")
     (style . "github"))
    (:hotel:
     (unicode . "🏨")
     (image . "hotel.png")
     (style . "github"))
    (:spy:
     (unicode . "🕵")
     (image . "spy.png")
     (style . "github"))
    (:flag-bz:
     (unicode . "🇧🇿")
     (image . "flag_bz.png")
     (style . "github"))
    (:athletic-shoe:
     (unicode . "👟")
     (image . "athletic_shoe.png")
     (style . "github"))
    (:rice_cracker:
     (unicode . "🍘")
     (image . "rice_cracker.png")
     (style . "github"))
    (:+1_tone3:
     (unicode . "👍🏽")
     (image . "thumbsup_tone3.png")
     (style . "github"))
    (:girl_tone1:
     (unicode . "👧🏻")
     (image . "girl_tone1.png")
     (style . "github"))
    (:dragon:
     (unicode . "🐉")
     (image . "dragon.png")
     (style . "github"))
    (:horse:
     (unicode . "🐴")
     (image . "horse.png")
     (style . "github"))
    (:kg:
     (unicode . "🇰🇬")
     (image . "flag_kg.png")
     (style . "github"))
    (:biohazard:
     (unicode . "☣")
     (image . "biohazard.png")
     (style . "github"))
    (:icecream:
     (unicode . "🍦")
     (image . "icecream.png")
     (style . "github"))
    (:dollar:
     (unicode . "💵")
     (image . "dollar.png")
     (style . "github"))
    (:ok_woman:
     (ascii . "*\\0/*")
     (unicode . "🙆")
     (image . "ok_woman.png")
     (style . "github"))
    (:flag-so:
     (unicode . "🇸🇴")
     (image . "flag_so.png")
     (style . "github"))
    (:flag-nf:
     (unicode . "🇳🇫")
     (image . "flag_nf.png")
     (style . "github"))
    (:dancer_tone5:
     (unicode . "💃🏿")
     (image . "dancer_tone5.png")
     (style . "github"))
    (:speak-no-evil:
     (unicode . "🙊")
     (image . "speak_no_evil.png")
     (style . "github"))
    (:flag-mn:
     (unicode . "🇲🇳")
     (image . "flag_mn.png")
     (style . "github"))
    (:waxing_crescent_moon:
     (unicode . "🌒")
     (image . "waxing_crescent_moon.png")
     (style . "github"))
    (:arrow_lower_left:
     (unicode . "↙")
     (image . "arrow_lower_left.png")
     (style . "github"))
    (:ie:
     (unicode . "🇮🇪")
     (image . "flag_ie.png")
     (style . "github"))
    (:earth-asia:
     (unicode . "🌏")
     (image . "earth_asia.png")
     (style . "github"))
    (:flag-bl:
     (unicode . "🇧🇱")
     (image . "flag_bl.png")
     (style . "github"))
    (:flag-lb:
     (unicode . "🇱🇧")
     (image . "flag_lb.png")
     (style . "github"))
    (:ok-hand-tone1:
     (unicode . "👌🏻")
     (image . "ok_hand_tone1.png")
     (style . "github"))
    (:ag:
     (unicode . "🇦🇬")
     (image . "flag_ag.png")
     (style . "github"))
    (:metal_tone1:
     (unicode . "🤘🏻")
     (image . "metal_tone1.png")
     (style . "github"))
    (:ky:
     (unicode . "🇰🇾")
     (image . "flag_ky.png")
     (style . "github"))
    (:file_folder:
     (unicode . "📁")
     (image . "file_folder.png")
     (style . "github"))
    (:frame_photo:
     (unicode . "🖼")
     (image . "frame_photo.png")
     (style . "github"))
    (:massage_tone1:
     (unicode . "💆🏻")
     (image . "massage_tone1.png")
     (style . "github"))
    (:sound:
     (unicode . "🔉")
     (image . "sound.png")
     (style . "github"))
    (:flag_sn:
     (unicode . "🇸🇳")
     (image . "flag_sn.png")
     (style . "github"))
    (:flag_fk:
     (unicode . "🇫🇰")
     (image . "flag_fk.png")
     (style . "github"))
    (:older_woman_tone2:
     (unicode . "👵🏼")
     (image . "older_woman_tone2.png")
     (style . "github"))
    (:keycap_asterisk:
     (unicode . "*⃣")
     (image . "asterisk.png")
     (style . "github"))
    (:flag-ax:
     (unicode . "🇦🇽")
     (image . "flag_ax.png")
     (style . "github"))
    (:point_up:
     (unicode . "☝")
     (image . "point_up.png")
     (style . "github"))
    (:flag_ba:
     (unicode . "🇧🇦")
     (image . "flag_ba.png")
     (style . "github"))
    (:au:
     (unicode . "🇦🇺")
     (image . "flag_au.png")
     (style . "github"))
    (:flag_tk:
     (unicode . "🇹🇰")
     (image . "flag_tk.png")
     (style . "github"))
    (:flag_ga:
     (unicode . "🇬🇦")
     (image . "flag_ga.png")
     (style . "github"))
    (:evergreen_tree:
     (unicode . "🌲")
     (image . "evergreen_tree.png")
     (style . "github"))
    (:globe_with_meridians:
     (unicode . "🌐")
     (image . "globe_with_meridians.png")
     (style . "github"))
    (:lower_left_fountain_pen:
     (unicode . "🖋")
     (image . "pen_fountain.png")
     (style . "github"))
    (:cy:
     (unicode . "🇨🇾")
     (image . "flag_cy.png")
     (style . "github"))
    (:play_pause:
     (unicode . "⏯")
     (image . "play_pause.png")
     (style . "github"))
    (:womans_hat:
     (unicode . "👒")
     (image . "womans_hat.png")
     (style . "github"))
    (:bride_with_veil_tone5:
     (unicode . "👰🏿")
     (image . "bride_with_veil_tone5.png")
     (style . "github"))
    (:arrow-double-up:
     (unicode . "⏫")
     (image . "arrow_double_up.png")
     (style . "github"))
    (:saxophone:
     (unicode . "🎷")
     (image . "saxophone.png")
     (style . "github"))
    (:thumbsdown_tone1:
     (unicode . "👎🏻")
     (image . "thumbsdown_tone1.png")
     (style . "github"))
    (:hand-splayed-tone2:
     (unicode . "🖐🏼")
     (image . "hand_splayed_tone2.png")
     (style . "github"))
    (:fast-forward:
     (unicode . "⏩")
     (image . "fast_forward.png")
     (style . "github"))
    (:new_moon:
     (unicode . "🌑")
     (image . "new_moon.png")
     (style . "github"))
    (:gq:
     (unicode . "🇬🇶")
     (image . "flag_gq.png")
     (style . "github"))
    (:flag-vg:
     (unicode . "🇻🇬")
     (image . "flag_vg.png")
     (style . "github"))
    (:information-source:
     (unicode . "ℹ")
     (image . "information_source.png")
     (style . "github"))
    (:thought-balloon:
     (unicode . "💭")
     (image . "thought_balloon.png")
     (style . "github"))
    (:flag-cr:
     (unicode . "🇨🇷")
     (image . "flag_cr.png")
     (style . "github"))
    (:bow_tone1:
     (unicode . "🙇🏻")
     (image . "bow_tone1.png")
     (style . "github"))
    (:lipstick:
     (unicode . "💄")
     (image . "lipstick.png")
     (style . "github"))
    (:fountain:
     (unicode . "⛲")
     (image . "fountain.png")
     (style . "github"))
    (:haircut_tone1:
     (unicode . "💇🏻")
     (image . "haircut_tone1.png")
     (style . "github"))
    (:ok_hand_tone1:
     (unicode . "👌🏻")
     (image . "ok_hand_tone1.png")
     (style . "github"))
    (:sleuth_or_spy_tone3:
     (unicode . "🕵🏽")
     (image . "spy_tone3.png")
     (style . "github"))
    (:pager:
     (unicode . "📟")
     (image . "pager.png")
     (style . "github"))
    (:family-wwg:
     (unicode . "👩👩👧")
     (image . "family_wwg.png")
     (style . "github"))
    (:surfer_tone1:
     (unicode . "🏄🏻")
     (image . "surfer_tone1.png")
     (style . "github"))
    (:mega:
     (unicode . "📣")
     (image . "mega.png")
     (style . "github"))
    (:clap_tone4:
     (unicode . "👏🏾")
     (image . "clap_tone4.png")
     (style . "github"))
    (:dagger_knife:
     (unicode . "🗡")
     (image . "dagger.png")
     (style . "github"))
    (:balloon:
     (unicode . "🎈")
     (image . "balloon.png")
     (style . "github"))
    (:truck:
     (unicode . "🚚")
     (image . "truck.png")
     (style . "github"))
    (:pencil2:
     (unicode . "✏")
     (image . "pencil2.png")
     (style . "github"))
    (:flag-bq:
     (unicode . "🇧🇶")
     (image . "flag_bq.png")
     (style . "github"))
    (:abc:
     (unicode . "🔤")
     (image . "abc.png")
     (style . "github"))
    (:reminder-ribbon:
     (unicode . "🎗")
     (image . "reminder_ribbon.png")
     (style . "github"))
    (:ar:
     (unicode . "🇦🇷")
     (image . "flag_ar.png")
     (style . "github"))
    (:money-with-wings:
     (unicode . "💸")
     (image . "money_with_wings.png")
     (style . "github"))
    (:flag_ai:
     (unicode . "🇦🇮")
     (image . "flag_ai.png")
     (style . "github"))
    (:man_with_turban_tone2:
     (unicode . "👳🏼")
     (image . "man_with_turban_tone2.png")
     (style . "github"))
    (:waning_crescent_moon:
     (unicode . "🌘")
     (image . "waning_crescent_moon.png")
     (style . "github"))
    (:th:
     (unicode . "🇹🇭")
     (image . "flag_th.png")
     (style . "github"))
    (:point-up-2-tone3:
     (unicode . "👆🏽")
     (image . "point_up_2_tone3.png")
     (style . "github"))
    (:hibiscus:
     (unicode . "🌺")
     (image . "hibiscus.png")
     (style . "github"))
    (:flag_md:
     (unicode . "🇲🇩")
     (image . "flag_md.png")
     (style . "github"))
    (:bow-tone3:
     (unicode . "🙇🏽")
     (image . "bow_tone3.png")
     (style . "github"))
    (:sy:
     (unicode . "🇸🇾")
     (image . "flag_sy.png")
     (style . "github"))
    (:flag-ss:
     (unicode . "🇸🇸")
     (image . "flag_ss.png")
     (style . "github"))
    (:nail_care:
     (unicode . "💅")
     (image . "nail_care.png")
     (style . "github"))
    (:raising_hand_tone2:
     (unicode . "🙋🏼")
     (image . "raising_hand_tone2.png")
     (style . "github"))
    (:flag_bw:
     (unicode . "🇧🇼")
     (image . "flag_bw.png")
     (style . "github"))
    (:man-with-turban-tone4:
     (unicode . "👳🏾")
     (image . "man_with_turban_tone4.png")
     (style . "github"))
    (:white-sun-cloud:
     (unicode . "🌥")
     (image . "white_sun_cloud.png")
     (style . "github"))
    (:flag_mp:
     (unicode . "🇲🇵")
     (image . "flag_mp.png")
     (style . "github"))
    (:head-bandage:
     (unicode . "🤕")
     (image . "head_bandage.png")
     (style . "github"))
    (:face_with_rolling_eyes:
     (unicode . "🙄")
     (image . "rolling_eyes.png")
     (style . "github"))
    (:lifter-tone3:
     (unicode . "🏋🏽")
     (image . "lifter_tone3.png")
     (style . "github"))
    (:kissing-closed-eyes:
     (unicode . "😚")
     (image . "kissing_closed_eyes.png")
     (style . "github"))
    (:flag-white:
     (unicode . "🏳")
     (image . "flag_white.png")
     (style . "github"))
    (:raising-hand-tone5:
     (unicode . "🙋🏿")
     (image . "raising_hand_tone5.png")
     (style . "github"))
    (:cricket:
     (unicode . "🏏")
     (image . "cricket.png")
     (style . "github"))
    (:middle_finger_tone1:
     (unicode . "🖕🏻")
     (image . "middle_finger_tone1.png")
     (style . "github"))
    (:vulcan_tone2:
     (unicode . "🖖🏼")
     (image . "vulcan_tone2.png")
     (style . "github"))
    (:hammer_pick:
     (unicode . "⚒")
     (image . "hammer_pick.png")
     (style . "github"))
    (:vulcan-tone4:
     (unicode . "🖖🏾")
     (image . "vulcan_tone4.png")
     (style . "github"))
    (:arrow-lower-left:
     (unicode . "↙")
     (image . "arrow_lower_left.png")
     (style . "github"))
    (:flag_vg:
     (unicode . "🇻🇬")
     (image . "flag_vg.png")
     (style . "github"))
    (:reminder_ribbon:
     (unicode . "🎗")
     (image . "reminder_ribbon.png")
     (style . "github"))
    (:flag-black:
     (unicode . "🏴")
     (image . "flag_black.png")
     (style . "github"))
    (:flag_tw:
     (unicode . "🇹🇼")
     (image . "flag_tw.png")
     (style . "github"))
    (:santa-tone4:
     (unicode . "🎅🏾")
     (image . "santa_tone4.png")
     (style . "github"))
    (:rabbit:
     (unicode . "🐰")
     (image . "rabbit.png")
     (style . "github"))
    (:homes:
     (unicode . "🏘")
     (image . "homes.png")
     (style . "github"))
    (:to:
     (unicode . "🇹🇴")
     (image . "flag_to.png")
     (style . "github"))
    (:clipboard:
     (unicode . "📋")
     (image . "clipboard.png")
     (style . "github"))
    (:no-bicycles:
     (unicode . "🚳")
     (image . "no_bicycles.png")
     (style . "github"))
    (:checkered_flag:
     (unicode . "🏁")
     (image . "checkered_flag.png")
     (style . "github"))
    (:cat:
     (unicode . "🐱")
     (image . "cat.png")
     (style . "github"))
    (:athletic_shoe:
     (unicode . "👟")
     (image . "athletic_shoe.png")
     (style . "github"))
    (:flag_cm:
     (unicode . "🇨🇲")
     (image . "flag_cm.png")
     (style . "github"))
    (:+1:
     (unicode . "👍")
     (image . "thumbsup.png")
     (style . "github"))
    (:newspaper2:
     (unicode . "🗞")
     (image . "newspaper2.png")
     (style . "github"))
    (:thumbsup:
     (unicode . "👍")
     (image . "thumbsup.png")
     (style . "github"))
    (:potable_water:
     (unicode . "🚰")
     (image . "potable_water.png")
     (style . "github"))
    (:man-tone2:
     (unicode . "👨🏼")
     (image . "man_tone2.png")
     (style . "github"))
    (:last-quarter-moon-with-face:
     (unicode . "🌜")
     (image . "last_quarter_moon_with_face.png")
     (style . "github"))
    (:ear_tone2:
     (unicode . "👂🏼")
     (image . "ear_tone2.png")
     (style . "github"))
    (:postal_horn:
     (unicode . "📯")
     (image . "postal_horn.png")
     (style . "github"))
    (:camera:
     (unicode . "📷")
     (image . "camera.png")
     (style . "github"))
    (:point_up_2_tone2:
     (unicode . "👆🏼")
     (image . "point_up_2_tone2.png")
     (style . "github"))
    (:envelope:
     (unicode . "✉")
     (image . "envelope.png")
     (style . "github"))
    (:ta:
     (unicode . "🇹🇦")
     (image . "flag_ta.png")
     (style . "github"))
    (:flag_rw:
     (unicode . "🇷🇼")
     (image . "flag_rw.png")
     (style . "github"))
    (:flag-id:
     (unicode . "🇮🇩")
     (image . "flag_id.png")
     (style . "github"))
    (:flag_ua:
     (unicode . "🇺🇦")
     (image . "flag_ua.png")
     (style . "github"))
    (:wink:
     (ascii . ";)")
     (unicode . "😉")
     (image . "wink.png")
     (style . "github"))
    (:straight_ruler:
     (unicode . "📏")
     (image . "straight_ruler.png")
     (style . "github"))
    (:metal-tone4:
     (unicode . "🤘🏾")
     (image . "metal_tone4.png")
     (style . "github"))
    (:bh:
     (unicode . "🇧🇭")
     (image . "flag_bh.png")
     (style . "github"))
    (:sailboat:
     (unicode . "⛵")
     (image . "sailboat.png")
     (style . "github"))
    (:vulcan:
     (unicode . "🖖")
     (image . "vulcan.png")
     (style . "github"))
    (:person-frowning-tone2:
     (unicode . "🙍🏼")
     (image . "person_frowning_tone2.png")
     (style . "github"))
    (:sk:
     (unicode . "🇸🇰")
     (image . "flag_sk.png")
     (style . "github"))
    (:upside-down:
     (unicode . "🙃")
     (image . "upside_down.png")
     (style . "github"))
    (:diamond-shape-with-a-dot-inside:
     (unicode . "💠")
     (image . "diamond_shape_with_a_dot_inside.png")
     (style . "github"))
    (:-1_tone3:
     (unicode . "👎🏽")
     (image . "thumbsdown_tone3.png")
     (style . "github"))
    (:flag_mr:
     (unicode . "🇲🇷")
     (image . "flag_mr.png")
     (style . "github"))
    (:no_good_tone2:
     (unicode . "🙅🏼")
     (image . "no_good_tone2.png")
     (style . "github"))
    (:bulb:
     (unicode . "💡")
     (image . "bulb.png")
     (style . "github"))
    (:outbox_tray:
     (unicode . "📤")
     (image . "outbox_tray.png")
     (style . "github"))
    (:wine-glass:
     (unicode . "🍷")
     (image . "wine_glass.png")
     (style . "github"))
    (:flag_re:
     (unicode . "🇷🇪")
     (image . "flag_re.png")
     (style . "github"))
    (:water-buffalo:
     (unicode . "🐃")
     (image . "water_buffalo.png")
     (style . "github"))
    (:classical_building:
     (unicode . "🏛")
     (image . "classical_building.png")
     (style . "github"))
    (:persevere:
     (ascii . ">.<")
     (unicode . "😣")
     (image . "persevere.png")
     (style . "github"))
    (:flag_pk:
     (unicode . "🇵🇰")
     (image . "flag_pk.png")
     (style . "github"))
    (:skier:
     (unicode . "⛷")
     (image . "skier.png")
     (style . "github"))
    (:candy:
     (unicode . "🍬")
     (image . "candy.png")
     (style . "github"))
    (:family-wwbb:
     (unicode . "👩👩👦👦")
     (image . "family_wwbb.png")
     (style . "github"))
    (:waning_gibbous_moon:
     (unicode . "🌖")
     (image . "waning_gibbous_moon.png")
     (style . "github"))
    (:cherry-blossom:
     (unicode . "🌸")
     (image . "cherry_blossom.png")
     (style . "github"))
    (:boy-tone1:
     (unicode . "👦🏻")
     (image . "boy_tone1.png")
     (style . "github"))
    (:seven:
     (unicode . "7⃣")
     (image . "seven.png")
     (style . "github"))
    (:microphone2:
     (unicode . "🎙")
     (image . "microphone2.png")
     (style . "github"))
    (:swimmer-tone1:
     (unicode . "🏊🏻")
     (image . "swimmer_tone1.png")
     (style . "github"))
    (:radioactive:
     (unicode . "☢")
     (image . "radioactive.png")
     (style . "github"))
    (:py:
     (unicode . "🇵🇾")
     (image . "flag_py.png")
     (style . "github"))
    (:tropical-fish:
     (unicode . "🐠")
     (image . "tropical_fish.png")
     (style . "github"))
    (:bridge_at_night:
     (unicode . "🌉")
     (image . "bridge_at_night.png")
     (style . "github"))
    (:flag_bl:
     (unicode . "🇧🇱")
     (image . "flag_bl.png")
     (style . "github"))
    (:broken-heart:
     (ascii . "</3")
     (unicode . "💔")
     (image . "broken_heart.png")
     (style . "github"))
    (:pray-tone1:
     (unicode . "🙏🏻")
     (image . "pray_tone1.png")
     (style . "github"))
    (:rowboat_tone3:
     (unicode . "🚣🏽")
     (image . "rowboat_tone3.png")
     (style . "github"))
    (:arrow_forward:
     (unicode . "▶")
     (image . "arrow_forward.png")
     (style . "github"))
    (:old_key:
     (unicode . "🗝")
     (image . "key2.png")
     (style . "github"))
    (:guardsman-tone4:
     (unicode . "💂🏾")
     (image . "guardsman_tone4.png")
     (style . "github"))
    (:information-desk-person-tone1:
     (unicode . "💁🏻")
     (image . "information_desk_person_tone1.png")
     (style . "github"))
    (:v_tone5:
     (unicode . "✌🏿")
     (image . "v_tone5.png")
     (style . "github"))
    (:hammer_and_wrench:
     (unicode . "🛠")
     (image . "tools.png")
     (style . "github"))
    (:open_mouth:
     (ascii . ":-O")
     (unicode . "😮")
     (image . "open_mouth.png")
     (style . "github"))
    (:flag-lu:
     (unicode . "🇱🇺")
     (image . "flag_lu.png")
     (style . "github"))
    (:cf:
     (unicode . "🇨🇫")
     (image . "flag_cf.png")
     (style . "github"))
    (:open-hands-tone4:
     (unicode . "👐🏾")
     (image . "open_hands_tone4.png")
     (style . "github"))
    (:small-orange-diamond:
     (unicode . "🔸")
     (image . "small_orange_diamond.png")
     (style . "github"))
    (:punch-tone3:
     (unicode . "👊🏽")
     (image . "punch_tone3.png")
     (style . "github"))
    (:ring:
     (unicode . "💍")
     (image . "ring.png")
     (style . "github"))
    (:mountain-snow:
     (unicode . "🏔")
     (image . "mountain_snow.png")
     (style . "github"))
    (:small_orange_diamond:
     (unicode . "🔸")
     (image . "small_orange_diamond.png")
     (style . "github"))
    (:flag_cp:
     (unicode . "🇨🇵")
     (image . "flag_cp.png")
     (style . "github"))
    (:ok-hand-tone4:
     (unicode . "👌🏾")
     (image . "ok_hand_tone4.png")
     (style . "github"))
    (:record_button:
     (unicode . "⏺")
     (image . "record_button.png")
     (style . "github"))
    (:lemon:
     (unicode . "🍋")
     (image . "lemon.png")
     (style . "github"))
    (:incoming_envelope:
     (unicode . "📨")
     (image . "incoming_envelope.png")
     (style . "github"))
    (:pen_fountain:
     (unicode . "🖋")
     (image . "pen_fountain.png")
     (style . "github"))
    (:wrench:
     (unicode . "🔧")
     (image . "wrench.png")
     (style . "github"))
    (:key2:
     (unicode . "🗝")
     (image . "key2.png")
     (style . "github"))
    (:u55b6:
     (unicode . "🈺")
     (image . "u55b6.png")
     (style . "github"))
    (:boy-tone3:
     (unicode . "👦🏽")
     (image . "boy_tone3.png")
     (style . "github"))
    (:np:
     (unicode . "🇳🇵")
     (image . "flag_np.png")
     (style . "github"))
    (:arrow_right_hook:
     (unicode . "↪")
     (image . "arrow_right_hook.png")
     (style . "github"))
    (:building_construction:
     (unicode . "🏗")
     (image . "construction_site.png")
     (style . "github"))
    (:heart-eyes:
     (unicode . "😍")
     (image . "heart_eyes.png")
     (style . "github"))
    (:sheep:
     (unicode . "🐑")
     (image . "sheep.png")
     (style . "github"))
    (:information_desk_person_tone3:
     (unicode . "💁🏽")
     (image . "information_desk_person_tone3.png")
     (style . "github"))
    (:space-invader:
     (unicode . "👾")
     (image . "space_invader.png")
     (style . "github"))
    (:heavy-plus-sign:
     (unicode . "➕")
     (image . "heavy_plus_sign.png")
     (style . "github"))
    (:fork_and_knife_with_plate:
     (unicode . "🍽")
     (image . "fork_knife_plate.png")
     (style . "github"))
    (:oncoming_bus:
     (unicode . "🚍")
     (image . "oncoming_bus.png")
     (style . "github"))
    (:zipper_mouth_face:
     (unicode . "🤐")
     (image . "zipper_mouth.png")
     (style . "github"))
    (:older-woman-tone5:
     (unicode . "👵🏿")
     (image . "older_woman_tone5.png")
     (style . "github"))
    (:rowboat-tone4:
     (unicode . "🚣🏾")
     (image . "rowboat_tone4.png")
     (style . "github"))
    (:suspension-railway:
     (unicode . "🚟")
     (image . "suspension_railway.png")
     (style . "github"))
    (:flag-nl:
     (unicode . "🇳🇱")
     (image . "flag_nl.png")
     (style . "github"))
    (:flag-mr:
     (unicode . "🇲🇷")
     (image . "flag_mr.png")
     (style . "github"))
    (:raised_hands_tone5:
     (unicode . "🙌🏿")
     (image . "raised_hands_tone5.png")
     (style . "github"))
    (:flag_kn:
     (unicode . "🇰🇳")
     (image . "flag_kn.png")
     (style . "github"))
    (:older_woman:
     (unicode . "👵")
     (image . "older_woman.png")
     (style . "github"))
    (:it:
     (unicode . "🇮🇹")
     (image . "flag_it.png")
     (style . "github"))
    (:bow-tone1:
     (unicode . "🙇🏻")
     (image . "bow_tone1.png")
     (style . "github"))
    (:ear-tone5:
     (unicode . "👂🏿")
     (image . "ear_tone5.png")
     (style . "github"))
    (:mountain_snow:
     (unicode . "🏔")
     (image . "mountain_snow.png")
     (style . "github"))
    (:sign_of_the_horns_tone1:
     (unicode . "🤘🏻")
     (image . "metal_tone1.png")
     (style . "github"))
    (:flag_np:
     (unicode . "🇳🇵")
     (image . "flag_np.png")
     (style . "github"))
    (:hand_splayed_tone2:
     (unicode . "🖐🏼")
     (image . "hand_splayed_tone2.png")
     (style . "github"))
    (:rice-ball:
     (unicode . "🍙")
     (image . "rice_ball.png")
     (style . "github"))
    (:kh:
     (unicode . "🇰🇭")
     (image . "flag_kh.png")
     (style . "github"))
    (:black-medium-small-square:
     (unicode . "◾")
     (image . "black_medium_small_square.png")
     (style . "github"))
    (:santa_tone3:
     (unicode . "🎅🏽")
     (image . "santa_tone3.png")
     (style . "github"))
    (:fist-tone2:
     (unicode . "✊🏼")
     (image . "fist_tone2.png")
     (style . "github"))
    (:cloud-rain:
     (unicode . "🌧")
     (image . "cloud_rain.png")
     (style . "github"))
    (:capricorn:
     (unicode . "♑")
     (image . "capricorn.png")
     (style . "github"))
    (:european_post_office:
     (unicode . "🏤")
     (image . "european_post_office.png")
     (style . "github"))
    (:mountain_bicyclist:
     (unicode . "🚵")
     (image . "mountain_bicyclist.png")
     (style . "github"))
    (:hotdog:
     (unicode . "🌭")
     (image . "hotdog.png")
     (style . "github"))
    (:flag-om:
     (unicode . "🇴🇲")
     (image . "flag_om.png")
     (style . "github"))
    (:mouse-three-button:
     (unicode . "🖱")
     (image . "mouse_three_button.png")
     (style . "github"))
    (:horse-racing-tone2:
     (unicode . "🏇🏼")
     (image . "horse_racing_tone2.png")
     (style . "github"))
    (:woman-tone1:
     (unicode . "👩🏻")
     (image . "woman_tone1.png")
     (style . "github"))
    (:flag-sn:
     (unicode . "🇸🇳")
     (image . "flag_sn.png")
     (style . "github"))
    (:grin:
     (unicode . "😁")
     (image . "grin.png")
     (style . "github"))
    (:fist_tone2:
     (unicode . "✊🏼")
     (image . "fist_tone2.png")
     (style . "github"))
    (:older-man-tone5:
     (unicode . "👴🏿")
     (image . "older_man_tone5.png")
     (style . "github"))
    (:family_mmb:
     (unicode . "👨👨👦")
     (image . "family_mmb.png")
     (style . "github"))
    (:head_bandage:
     (unicode . "🤕")
     (image . "head_bandage.png")
     (style . "github"))
    (:beginner:
     (unicode . "🔰")
     (image . "beginner.png")
     (style . "github"))
    (:ws:
     (unicode . "🇼🇸")
     (image . "flag_ws.png")
     (style . "github"))
    (:wedding:
     (unicode . "💒")
     (image . "wedding.png")
     (style . "github"))
    (:walking:
     (unicode . "🚶")
     (image . "walking.png")
     (style . "github"))
    (:information-desk-person-tone3:
     (unicode . "💁🏽")
     (image . "information_desk_person_tone3.png")
     (style . "github"))
    (:flag_nf:
     (unicode . "🇳🇫")
     (image . "flag_nf.png")
     (style . "github"))
    (:point-up-tone4:
     (unicode . "☝🏾")
     (image . "point_up_tone4.png")
     (style . "github"))
    (:currency-exchange:
     (unicode . "💱")
     (image . "currency_exchange.png")
     (style . "github"))
    (:v-tone3:
     (unicode . "✌🏽")
     (image . "v_tone3.png")
     (style . "github"))
    (:secret:
     (unicode . "㊙")
     (image . "secret.png")
     (style . "github"))
    (:ch:
     (unicode . "🇨🇭")
     (image . "flag_ch.png")
     (style . "github"))
    (:flag-ai:
     (unicode . "🇦🇮")
     (image . "flag_ai.png")
     (style . "github"))
    (:cityscape:
     (unicode . "🏙")
     (image . "cityscape.png")
     (style . "github"))
    (:horse-racing-tone3:
     (unicode . "🏇🏽")
     (image . "horse_racing_tone3.png")
     (style . "github"))
    (:love_hotel:
     (unicode . "🏩")
     (image . "love_hotel.png")
     (style . "github"))
    (:flag_lv:
     (unicode . "🇱🇻")
     (image . "flag_lv.png")
     (style . "github"))
    (:flag_si:
     (unicode . "🇸🇮")
     (image . "flag_si.png")
     (style . "github"))
    (:flag-fm:
     (unicode . "🇫🇲")
     (image . "flag_fm.png")
     (style . "github"))
    (:man-with-gua-pi-mao-tone1:
     (unicode . "👲🏻")
     (image . "man_with_gua_pi_mao_tone1.png")
     (style . "github"))
    (:rolling_eyes:
     (unicode . "🙄")
     (image . "rolling_eyes.png")
     (style . "github"))
    (:flag-lc:
     (unicode . "🇱🇨")
     (image . "flag_lc.png")
     (style . "github"))
    (:flag_gn:
     (unicode . "🇬🇳")
     (image . "flag_gn.png")
     (style . "github"))
    (:cupid:
     (unicode . "💘")
     (image . "cupid.png")
     (style . "github"))
    (:et:
     (unicode . "🇪🇹")
     (image . "flag_et.png")
     (style . "github"))
    (:older_woman_tone5:
     (unicode . "👵🏿")
     (image . "older_woman_tone5.png")
     (style . "github"))
    (:star_and_crescent:
     (unicode . "☪")
     (image . "star_and_crescent.png")
     (style . "github"))
    (:flag-jm:
     (unicode . "🇯🇲")
     (image . "flag_jm.png")
     (style . "github"))
    (:file-cabinet:
     (unicode . "🗄")
     (image . "file_cabinet.png")
     (style . "github"))
    (:house-abandoned:
     (unicode . "🏚")
     (image . "house_abandoned.png")
     (style . "github"))
    (:unicorn:
     (unicode . "🦄")
     (image . "unicorn.png")
     (style . "github"))
    (:latin_cross:
     (unicode . "✝")
     (image . "cross.png")
     (style . "github"))
    (:arrow-forward:
     (unicode . "▶")
     (image . "arrow_forward.png")
     (style . "github"))
    (:cherries:
     (unicode . "🍒")
     (image . "cherries.png")
     (style . "github"))
    (:loop:
     (unicode . "➿")
     (image . "loop.png")
     (style . "github"))
    (:yt:
     (unicode . "🇾🇹")
     (image . "flag_yt.png")
     (style . "github"))
    (:dvd:
     (unicode . "📀")
     (image . "dvd.png")
     (style . "github"))
    (:cz:
     (unicode . "🇨🇿")
     (image . "flag_cz.png")
     (style . "github"))
    (:asterisk:
     (unicode . "*⃣")
     (image . "asterisk.png")
     (style . "github"))
    (:bath-tone3:
     (unicode . "🛀🏽")
     (image . "bath_tone3.png")
     (style . "github"))
    (:flag-es:
     (unicode . "🇪🇸")
     (image . "flag_es.png")
     (style . "github"))
    (:sunrise-over-mountains:
     (unicode . "🌄")
     (image . "sunrise_over_mountains.png")
     (style . "github"))
    (:white-small-square:
     (unicode . "▫")
     (image . "white_small_square.png")
     (style . "github"))
    (:hand-splayed-tone3:
     (unicode . "🖐🏽")
     (image . "hand_splayed_tone3.png")
     (style . "github"))
    (:gr:
     (unicode . "🇬🇷")
     (image . "flag_gr.png")
     (style . "github"))
    (:baby_chick:
     (unicode . "🐤")
     (image . "baby_chick.png")
     (style . "github"))
    (:steam_locomotive:
     (unicode . "🚂")
     (image . "steam_locomotive.png")
     (style . "github"))
    (:middle-finger-tone4:
     (unicode . "🖕🏾")
     (image . "middle_finger_tone4.png")
     (style . "github"))
    (:mg:
     (unicode . "🇲🇬")
     (image . "flag_mg.png")
     (style . "github"))
    (:man_tone3:
     (unicode . "👨🏽")
     (image . "man_tone3.png")
     (style . "github"))
    (:whale2:
     (unicode . "🐋")
     (image . "whale2.png")
     (style . "github"))
    (:white_large_square:
     (unicode . "⬜")
     (image . "white_large_square.png")
     (style . "github"))
    (:dress:
     (unicode . "👗")
     (image . "dress.png")
     (style . "github"))
    (:man_with_turban_tone3:
     (unicode . "👳🏽")
     (image . "man_with_turban_tone3.png")
     (style . "github"))
    (:bow_tone2:
     (unicode . "🙇🏼")
     (image . "bow_tone2.png")
     (style . "github"))
    (:yin-yang:
     (unicode . "☯")
     (image . "yin_yang.png")
     (style . "github"))
    (:scroll:
     (unicode . "📜")
     (image . "scroll.png")
     (style . "github"))
    (:sleepy:
     (unicode . "😪")
     (image . "sleepy.png")
     (style . "github"))
    (:sweat:
     (ascii . "':(")
     (unicode . "😓")
     (image . "sweat.png")
     (style . "github"))
    (:raising-hand-tone3:
     (unicode . "🙋🏽")
     (image . "raising_hand_tone3.png")
     (style . "github"))
    (:clap_tone5:
     (unicode . "👏🏿")
     (image . "clap_tone5.png")
     (style . "github"))
    (:passport-control:
     (unicode . "🛂")
     (image . "passport_control.png")
     (style . "github"))
    (:ni:
     (unicode . "🇳🇮")
     (image . "flag_ni.png")
     (style . "github"))
    (:gl:
     (unicode . "🇬🇱")
     (image . "flag_gl.png")
     (style . "github"))
    (:one:
     (unicode . "1⃣")
     (image . "one.png")
     (style . "github"))
    (:flag-gi:
     (unicode . "🇬🇮")
     (image . "flag_gi.png")
     (style . "github"))
    (:flag-al:
     (unicode . "🇦🇱")
     (image . "flag_al.png")
     (style . "github"))
    (:gd:
     (unicode . "🇬🇩")
     (image . "flag_gd.png")
     (style . "github"))
    (:black_medium_square:
     (unicode . "◼")
     (image . "black_medium_square.png")
     (style . "github"))
    (:gift:
     (unicode . "🎁")
     (image . "gift.png")
     (style . "github"))
    (:v:
     (unicode . "✌")
     (image . "v.png")
     (style . "github"))
    (:running_shirt_with_sash:
     (unicode . "🎽")
     (image . "running_shirt_with_sash.png")
     (style . "github"))
    (:play-pause:
     (unicode . "⏯")
     (image . "play_pause.png")
     (style . "github"))
    (:small_airplane:
     (unicode . "🛩")
     (image . "airplane_small.png")
     (style . "github"))
    (:raising_hand_tone1:
     (unicode . "🙋🏻")
     (image . "raising_hand_tone1.png")
     (style . "github"))
    (:flag_mc:
     (unicode . "🇲🇨")
     (image . "flag_mc.png")
     (style . "github"))
    (:sz:
     (unicode . "🇸🇿")
     (image . "flag_sz.png")
     (style . "github"))
    (:racing_car:
     (unicode . "🏎")
     (image . "race_car.png")
     (style . "github"))
    (:stuck_out_tongue:
     (ascii . ":P")
     (unicode . "😛")
     (image . "stuck_out_tongue.png")
     (style . "github"))
    (:walking-tone3:
     (unicode . "🚶🏽")
     (image . "walking_tone3.png")
     (style . "github"))
    (:nail-care-tone1:
     (unicode . "💅🏻")
     (image . "nail_care_tone1.png")
     (style . "github"))
    (:eye_in_speech_bubble:
     (unicode . "👁🗨")
     (image . "eye_in_speech_bubble.png")
     (style . "github"))
    (:busstop:
     (unicode . "🚏")
     (image . "busstop.png")
     (style . "github"))
    (:frame-photo:
     (unicode . "🖼")
     (image . "frame_photo.png")
     (style . "github"))
    (:flag-sr:
     (unicode . "🇸🇷")
     (image . "flag_sr.png")
     (style . "github"))
    (:railroad_track:
     (unicode . "🛤")
     (image . "railway_track.png")
     (style . "github"))
    (:bank:
     (unicode . "🏦")
     (image . "bank.png")
     (style . "github"))
    (:construction-worker-tone1:
     (unicode . "👷🏻")
     (image . "construction_worker_tone1.png")
     (style . "github"))
    (:muscle-tone2:
     (unicode . "💪🏼")
     (image . "muscle_tone2.png")
     (style . "github"))
    (:bath_tone4:
     (unicode . "🛀🏾")
     (image . "bath_tone4.png")
     (style . "github"))
    (:b:
     (unicode . "🅱")
     (image . "b.png")
     (style . "github"))
    (:island:
     (unicode . "🏝")
     (image . "island.png")
     (style . "github"))
    (:flag_bt:
     (unicode . "🇧🇹")
     (image . "flag_bt.png")
     (style . "github"))
    (:scissors:
     (unicode . "✂")
     (image . "scissors.png")
     (style . "github"))
    (:flag_tf:
     (unicode . "🇹🇫")
     (image . "flag_tf.png")
     (style . "github"))
    (:lv:
     (unicode . "🇱🇻")
     (image . "flag_lv.png")
     (style . "github"))
    (:articulated-lorry:
     (unicode . "🚛")
     (image . "articulated_lorry.png")
     (style . "github"))
    (:horse_racing_tone5:
     (unicode . "🏇🏿")
     (image . "horse_racing_tone5.png")
     (style . "github"))
    (:birthday:
     (unicode . "🎂")
     (image . "birthday.png")
     (style . "github"))
    (:monkey:
     (unicode . "🐒")
     (image . "monkey.png")
     (style . "github"))
    (:sake:
     (unicode . "🍶")
     (image . "sake.png")
     (style . "github"))
    (:raised-hands:
     (unicode . "🙌")
     (image . "raised_hands.png")
     (style . "github"))
    (:man-with-turban-tone5:
     (unicode . "👳🏿")
     (image . "man_with_turban_tone5.png")
     (style . "github"))
    (:dancer_tone3:
     (unicode . "💃🏽")
     (image . "dancer_tone3.png")
     (style . "github"))
    (:no_entry:
     (unicode . "⛔")
     (image . "no_entry.png")
     (style . "github"))
    (:low_brightness:
     (unicode . "🔅")
     (image . "low_brightness.png")
     (style . "github"))
    (:pause_button:
     (unicode . "⏸")
     (image . "pause_button.png")
     (style . "github"))
    (:loudspeaker:
     (unicode . "📢")
     (image . "loudspeaker.png")
     (style . "github"))
    (:angel_tone3:
     (unicode . "👼🏽")
     (image . "angel_tone3.png")
     (style . "github"))
    (:arrow_heading_up:
     (unicode . "⤴")
     (image . "arrow_heading_up.png")
     (style . "github"))
    (:joy_cat:
     (unicode . "😹")
     (image . "joy_cat.png")
     (style . "github"))
    (:construction:
     (unicode . "🚧")
     (image . "construction.png")
     (style . "github"))
    (:baby-tone4:
     (unicode . "👶🏾")
     (image . "baby_tone4.png")
     (style . "github"))
    (:top:
     (unicode . "🔝")
     (image . "top.png")
     (style . "github"))
    (:globe-with-meridians:
     (unicode . "🌐")
     (image . "globe_with_meridians.png")
     (style . "github"))
    (:man-tone3:
     (unicode . "👨🏽")
     (image . "man_tone3.png")
     (style . "github"))
    (:tn:
     (unicode . "🇹🇳")
     (image . "flag_tn.png")
     (style . "github"))
    (:video-camera:
     (unicode . "📹")
     (image . "video_camera.png")
     (style . "github"))
    (:jo:
     (unicode . "🇯🇴")
     (image . "flag_jo.png")
     (style . "github"))
    (:nut_and_bolt:
     (unicode . "🔩")
     (image . "nut_and_bolt.png")
     (style . "github"))
    (:star:
     (unicode . "⭐")
     (image . "star.png")
     (style . "github"))
    (:vn:
     (unicode . "🇻🇳")
     (image . "flag_vn.png")
     (style . "github"))
    (:microphone:
     (unicode . "🎤")
     (image . "microphone.png")
     (style . "github"))
    (:golfer:
     (unicode . "🏌")
     (image . "golfer.png")
     (style . "github"))
    (:family_mmbb:
     (unicode . "👨👨👦👦")
     (image . "family_mmbb.png")
     (style . "github"))
    (:cancer:
     (unicode . "♋")
     (image . "cancer.png")
     (style . "github"))
    (:sagittarius:
     (unicode . "♐")
     (image . "sagittarius.png")
     (style . "github"))
    (:flag-cg:
     (unicode . "🇨🇬")
     (image . "flag_cg.png")
     (style . "github"))
    (:flag_cl:
     (unicode . "🇨🇱")
     (image . "flag_cl.png")
     (style . "github"))
    (:bellhop:
     (unicode . "🛎")
     (image . "bellhop.png")
     (style . "github"))
    (:couple-with-heart:
     (unicode . "💑")
     (image . "couple_with_heart.png")
     (style . "github"))
    (:sos:
     (unicode . "🆘")
     (image . "sos.png")
     (style . "github"))
    (:ram:
     (unicode . "🐏")
     (image . "ram.png")
     (style . "github"))
    (:flag-kp:
     (unicode . "🇰🇵")
     (image . "flag_kp.png")
     (style . "github"))
    (:slightly_smiling_face:
     (unicode . "🙂")
     (image . "slight_smile.png")
     (style . "github"))
    (:mag:
     (unicode . "🔍")
     (image . "mag.png")
     (style . "github"))
    (:o:
     (unicode . "⭕")
     (image . "o.png")
     (style . "github"))
    (:flag-ki:
     (unicode . "🇰🇮")
     (image . "flag_ki.png")
     (style . "github"))
    (:pen-ballpoint:
     (unicode . "🖊")
     (image . "pen_ballpoint.png")
     (style . "github"))
    (:person-frowning-tone3:
     (unicode . "🙍🏽")
     (image . "person_frowning_tone3.png")
     (style . "github"))
    (:flag_om:
     (unicode . "🇴🇲")
     (image . "flag_om.png")
     (style . "github"))
    (:sl:
     (unicode . "🇸🇱")
     (image . "flag_sl.png")
     (style . "github"))
    (:flag_de:
     (unicode . "🇩🇪")
     (image . "flag_de.png")
     (style . "github"))
    (:accept:
     (unicode . "🉑")
     (image . "accept.png")
     (style . "github"))
    (:electric_plug:
     (unicode . "🔌")
     (image . "electric_plug.png")
     (style . "github"))
    (:no_good_tone1:
     (unicode . "🙅🏻")
     (image . "no_good_tone1.png")
     (style . "github"))
    (:flag_mq:
     (unicode . "🇲🇶")
     (image . "flag_mq.png")
     (style . "github"))
    (:construction_worker_tone1:
     (unicode . "👷🏻")
     (image . "construction_worker_tone1.png")
     (style . "github"))
    (:ambulance:
     (unicode . "🚑")
     (image . "ambulance.png")
     (style . "github"))
    (:couple_with_heart_mm:
     (unicode . "👨❤👨")
     (image . "couple_mm.png")
     (style . "github"))
    (:cruise-ship:
     (unicode . "🛳")
     (image . "cruise_ship.png")
     (style . "github"))
    (:black_nib:
     (unicode . "✒")
     (image . "black_nib.png")
     (style . "github"))
    (:ice_skate:
     (unicode . "⛸")
     (image . "ice_skate.png")
     (style . "github"))
    (:womens:
     (unicode . "🚺")
     (image . "womens.png")
     (style . "github"))
    (:key:
     (unicode . "🔑")
     (image . "key.png")
     (style . "github"))
    (:sunrise:
     (unicode . "🌅")
     (image . "sunrise.png")
     (style . "github"))
    (:outbox-tray:
     (unicode . "📤")
     (image . "outbox_tray.png")
     (style . "github"))
    (:boy-tone2:
     (unicode . "👦🏼")
     (image . "boy_tone2.png")
     (style . "github"))
    (:no_mouth:
     (ascii . ":-X")
     (unicode . "😶")
     (image . "no_mouth.png")
     (style . "github"))
    (:middle-finger-tone3:
     (unicode . "🖕🏽")
     (image . "middle_finger_tone3.png")
     (style . "github"))
    (:vibration-mode:
     (unicode . "📳")
     (image . "vibration_mode.png")
     (style . "github"))
    (:flag-fr:
     (unicode . "🇫🇷")
     (image . "flag_fr.png")
     (style . "github"))
    (:flag_ws:
     (unicode . "🇼🇸")
     (image . "flag_ws.png")
     (style . "github"))
    (:calendar_spiral:
     (unicode . "🗓")
     (image . "calendar_spiral.png")
     (style . "github"))
    (:oncoming-taxi:
     (unicode . "🚖")
     (image . "oncoming_taxi.png")
     (style . "github"))
    (:flag-ge:
     (unicode . "🇬🇪")
     (image . "flag_ge.png")
     (style . "github"))
    (:hand_splayed_tone1:
     (unicode . "🖐🏻")
     (image . "hand_splayed_tone1.png")
     (style . "github"))
    (:green-book:
     (unicode . "📗")
     (image . "green_book.png")
     (style . "github"))
    (:information-desk-person-tone2:
     (unicode . "💁🏼")
     (image . "information_desk_person_tone2.png")
     (style . "github"))
    (:flag_zm:
     (unicode . "🇿🇲")
     (image . "flag_zm.png")
     (style . "github"))
    (:guardsman-tone5:
     (unicode . "💂🏿")
     (image . "guardsman_tone5.png")
     (style . "github"))
    (:kissing-heart:
     (ascii . ":*")
     (unicode . "😘")
     (image . "kissing_heart.png")
     (style . "github"))
    (:point_down_tone3:
     (unicode . "👇🏽")
     (image . "point_down_tone3.png")
     (style . "github"))
    (:cg:
     (unicode . "🇨🇬")
     (image . "flag_cg.png")
     (style . "github"))
    (:couplekiss:
     (unicode . "💏")
     (image . "couplekiss.png")
     (style . "github"))
    (:open-hands-tone3:
     (unicode . "👐🏽")
     (image . "open_hands_tone3.png")
     (style . "github"))
    (:flag_ni:
     (unicode . "🇳🇮")
     (image . "flag_ni.png")
     (style . "github"))
    (:zw:
     (unicode . "🇿🇼")
     (image . "flag_zw.png")
     (style . "github"))
    (:person-with-pouting-face-tone3:
     (unicode . "🙎🏽")
     (image . "person_with_pouting_face_tone3.png")
     (style . "github"))
    (:ok-hand-tone5:
     (unicode . "👌🏿")
     (image . "ok_hand_tone5.png")
     (style . "github"))
    (:hospital:
     (unicode . "🏥")
     (image . "hospital.png")
     (style . "github"))
    (:bust-in-silhouette:
     (unicode . "👤")
     (image . "bust_in_silhouette.png")
     (style . "github"))
    (:slight-frown:
     (unicode . "🙁")
     (image . "slight_frown.png")
     (style . "github"))
    (:fk:
     (unicode . "🇫🇰")
     (image . "flag_fk.png")
     (style . "github"))
    (:department-store:
     (unicode . "🏬")
     (image . "department_store.png")
     (style . "github"))
    (:flag-pn:
     (unicode . "🇵🇳")
     (image . "flag_pn.png")
     (style . "github"))
    (:middle-finger:
     (unicode . "🖕")
     (image . "middle_finger.png")
     (style . "github"))
    (:zipper_mouth:
     (unicode . "🤐")
     (image . "zipper_mouth.png")
     (style . "github"))
    (:mailbox:
     (unicode . "📫")
     (image . "mailbox.png")
     (style . "github"))
    (:school_satchel:
     (unicode . "🎒")
     (image . "school_satchel.png")
     (style . "github"))
    (:hk:
     (unicode . "🇭🇰")
     (image . "flag_hk.png")
     (style . "github"))
    (:surfer-tone1:
     (unicode . "🏄🏻")
     (image . "surfer_tone1.png")
     (style . "github"))
    (:thumbsdown-tone5:
     (unicode . "👎🏿")
     (image . "thumbsdown_tone5.png")
     (style . "github"))
    (:tr:
     (unicode . "🇹🇷")
     (image . "flag_tr.png")
     (style . "github"))
    (:boom:
     (unicode . "💥")
     (image . "boom.png")
     (style . "github"))
    (:previous_track:
     (unicode . "⏮")
     (image . "track_previous.png")
     (style . "github"))
    (:raised-hands-tone5:
     (unicode . "🙌🏿")
     (image . "raised_hands_tone5.png")
     (style . "github"))
    (:guardsman_tone4:
     (unicode . "💂🏾")
     (image . "guardsman_tone4.png")
     (style . "github"))
    (:family-wwgb:
     (unicode . "👩👩👧👦")
     (image . "family_wwgb.png")
     (style . "github"))
    (:money-mouth:
     (unicode . "🤑")
     (image . "money_mouth.png")
     (style . "github"))
    (:minibus:
     (unicode . "🚐")
     (image . "minibus.png")
     (style . "github"))
    (:cloud-snow:
     (unicode . "🌨")
     (image . "cloud_snow.png")
     (style . "github"))
    (:person_with_ball_tone4:
     (unicode . "⛹🏾")
     (image . "basketball_player_tone4.png")
     (style . "github"))
    (:older-woman:
     (unicode . "👵")
     (image . "older_woman.png")
     (style . "github"))
    (:arrow_heading_down:
     (unicode . "⤵")
     (image . "arrow_heading_down.png")
     (style . "github"))
    (:clock930:
     (unicode . "🕤")
     (image . "clock930.png")
     (style . "github"))
    (:raised_hands_tone4:
     (unicode . "🙌🏾")
     (image . "raised_hands_tone4.png")
     (style . "github"))
    (:black-small-square:
     (unicode . "▪")
     (image . "black_small_square.png")
     (style . "github"))
    (:high-brightness:
     (unicode . "🔆")
     (image . "high_brightness.png")
     (style . "github"))
    (:railway-track:
     (unicode . "🛤")
     (image . "railway_track.png")
     (style . "github"))
    (:dancer_tone2:
     (unicode . "💃🏼")
     (image . "dancer_tone2.png")
     (style . "github"))
    (:two-men-holding-hands:
     (unicode . "👬")
     (image . "two_men_holding_hands.png")
     (style . "github"))
    (:ki:
     (unicode . "🇰🇮")
     (image . "flag_ki.png")
     (style . "github"))
    (:lower_left_ballpoint_pen:
     (unicode . "🖊")
     (image . "pen_ballpoint.png")
     (style . "github"))
    (:dark_sunglasses:
     (unicode . "🕶")
     (image . "dark_sunglasses.png")
     (style . "github"))
    (:man_with_gua_pi_mao_tone2:
     (unicode . "👲🏼")
     (image . "man_with_gua_pi_mao_tone2.png")
     (style . "github"))
    (:ma:
     (unicode . "🇲🇦")
     (image . "flag_ma.png")
     (style . "github"))
    (:haircut-tone5:
     (unicode . "💇🏿")
     (image . "haircut_tone5.png")
     (style . "github"))
    (:mailbox_with_no_mail:
     (unicode . "📭")
     (image . "mailbox_with_no_mail.png")
     (style . "github"))
    (:spider-web:
     (unicode . "🕸")
     (image . "spider_web.png")
     (style . "github"))
    (:airplane:
     (unicode . "✈")
     (image . "airplane.png")
     (style . "github"))
    (:arrow-backward:
     (unicode . "◀")
     (image . "arrow_backward.png")
     (style . "github"))
    (:fist_tone3:
     (unicode . "✊🏽")
     (image . "fist_tone3.png")
     (style . "github"))
    (:family_wwg:
     (unicode . "👩👩👧")
     (image . "family_wwg.png")
     (style . "github"))
    (:chile:
     (unicode . "🇨🇱")
     (image . "flag_cl.png")
     (style . "github"))
    (:flag-sm:
     (unicode . "🇸🇲")
     (image . "flag_sm.png")
     (style . "github"))
    (:page-with-curl:
     (unicode . "📃")
     (image . "page_with_curl.png")
     (style . "github"))
    (:massage_tone3:
     (unicode . "💆🏽")
     (image . "massage_tone3.png")
     (style . "github"))
    (:white_small_square:
     (unicode . "▫")
     (image . "white_small_square.png")
     (style . "github"))
    (:reversed_hand_with_middle_finger_extended_tone5:
     (unicode . "🖕🏿")
     (image . "middle_finger_tone5.png")
     (style . "github"))
    (:rewind:
     (unicode . "⏪")
     (image . "rewind.png")
     (style . "github"))
    (:flag-ua:
     (unicode . "🇺🇦")
     (image . "flag_ua.png")
     (style . "github"))
    (:snowboarder:
     (unicode . "🏂")
     (image . "snowboarder.png")
     (style . "github"))
    (:flag-bn:
     (unicode . "🇧🇳")
     (image . "flag_bn.png")
     (style . "github"))
    (:flag_gq:
     (unicode . "🇬🇶")
     (image . "flag_gq.png")
     (style . "github"))
    (:sleuth_or_spy_tone1:
     (unicode . "🕵🏻")
     (image . "spy_tone1.png")
     (style . "github"))
    (:star-and-crescent:
     (unicode . "☪")
     (image . "star_and_crescent.png")
     (style . "github"))
    (:smile:
     (ascii . ":)")
     (unicode . "😄")
     (image . "smile.png")
     (style . "github"))
    (:flag_ng:
     (unicode . "🇳🇬")
     (image . "flag_ng.png")
     (style . "github"))
    (:signal_strength:
     (unicode . "📶")
     (image . "signal_strength.png")
     (style . "github"))
    (:ci:
     (unicode . "🇨🇮")
     (image . "flag_ci.png")
     (style . "github"))
    (:older_woman_tone4:
     (unicode . "👵🏾")
     (image . "older_woman_tone4.png")
     (style . "github"))
    (:thumbsup-tone4:
     (unicode . "👍🏾")
     (image . "thumbsup_tone4.png")
     (style . "github"))
    (:flag_fi:
     (unicode . "🇫🇮")
     (image . "flag_fi.png")
     (style . "github"))
    (:oncoming_police_car:
     (unicode . "🚔")
     (image . "oncoming_police_car.png")
     (style . "github"))
    (:flag_sh:
     (unicode . "🇸🇭")
     (image . "flag_sh.png")
     (style . "github"))
    (:eu:
     (unicode . "🇪🇺")
     (image . "flag_eu.png")
     (style . "github"))
    (:flag_za:
     (unicode . "🇿🇦")
     (image . "flag_za.png")
     (style . "github"))
    (:flag-az:
     (unicode . "🇦🇿")
     (image . "flag_az.png")
     (style . "github"))
    (:pray_tone2:
     (unicode . "🙏🏼")
     (image . "pray_tone2.png")
     (style . "github"))
    (:shaved-ice:
     (unicode . "🍧")
     (image . "shaved_ice.png")
     (style . "github"))
    (:fried_shrimp:
     (unicode . "🍤")
     (image . "fried_shrimp.png")
     (style . "github"))
    (:oncoming_taxi:
     (unicode . "🚖")
     (image . "oncoming_taxi.png")
     (style . "github"))
    (:triangular_flag_on_post:
     (unicode . "🚩")
     (image . "triangular_flag_on_post.png")
     (style . "github"))
    (:man_with_turban:
     (unicode . "👳")
     (image . "man_with_turban.png")
     (style . "github"))
    (:za:
     (unicode . "🇿🇦")
     (image . "flag_za.png")
     (style . "github"))
    (:woman_tone1:
     (unicode . "👩🏻")
     (image . "woman_tone1.png")
     (style . "github"))
    (:mountain_bicyclist_tone2:
     (unicode . "🚵🏼")
     (image . "mountain_bicyclist_tone2.png")
     (style . "github"))
    (:blue_heart:
     (unicode . "💙")
     (image . "blue_heart.png")
     (style . "github"))
    (:bath-tone2:
     (unicode . "🛀🏼")
     (image . "bath_tone2.png")
     (style . "github"))
    (:thinking_face:
     (unicode . "🤔")
     (image . "thinking.png")
     (style . "github"))
    (:thermometer-face:
     (unicode . "🤒")
     (image . "thermometer_face.png")
     (style . "github"))
    (:page_with_curl:
     (unicode . "📃")
     (image . "page_with_curl.png")
     (style . "github"))
    (:wheel-of-dharma:
     (unicode . "☸")
     (image . "wheel_of_dharma.png")
     (style . "github"))
    (:calling:
     (unicode . "📲")
     (image . "calling.png")
     (style . "github"))
    (:no_good:
     (unicode . "🙅")
     (image . "no_good.png")
     (style . "github"))
    (:thumbsdown_tone2:
     (unicode . "👎🏼")
     (image . "thumbsdown_tone2.png")
     (style . "github"))
    (:clap:
     (unicode . "👏")
     (image . "clap.png")
     (style . "github"))
    (:smiling_imp:
     (unicode . "😈")
     (image . "smiling_imp.png")
     (style . "github"))
    (:gs:
     (unicode . "🇬🇸")
     (image . "flag_gs.png")
     (style . "github"))
    (:telescope:
     (unicode . "🔭")
     (image . "telescope.png")
     (style . "github"))
    (:broken_heart:
     (ascii . "</3")
     (unicode . "💔")
     (image . "broken_heart.png")
     (style . "github"))
    (:surfer_tone3:
     (unicode . "🏄🏽")
     (image . "surfer_tone3.png")
     (style . "github"))
    (:point-right-tone5:
     (unicode . "👉🏿")
     (image . "point_right_tone5.png")
     (style . "github"))
    (:flag-cp:
     (unicode . "🇨🇵")
     (image . "flag_cp.png")
     (style . "github"))
    (:flag-va:
     (unicode . "🇻🇦")
     (image . "flag_va.png")
     (style . "github"))
    (:metal_tone3:
     (unicode . "🤘🏽")
     (image . "metal_tone3.png")
     (style . "github"))
    (:bow:
     (unicode . "🙇")
     (image . "bow.png")
     (style . "github"))
    (:triangular_ruler:
     (unicode . "📐")
     (image . "triangular_ruler.png")
     (style . "github"))
    (:clap_tone2:
     (unicode . "👏🏼")
     (image . "clap_tone2.png")
     (style . "github"))
    (:circus-tent:
     (unicode . "🎪")
     (image . "circus_tent.png")
     (style . "github"))
    (:raising-hand-tone2:
     (unicode . "🙋🏼")
     (image . "raising_hand_tone2.png")
     (style . "github"))
    (:haircut_tone3:
     (unicode . "💇🏽")
     (image . "haircut_tone3.png")
     (style . "github"))
    (:woman:
     (unicode . "👩")
     (image . "woman.png")
     (style . "github"))
    (:flag-sc:
     (unicode . "🇸🇨")
     (image . "flag_sc.png")
     (style . "github"))
    (:part-alternation-mark:
     (unicode . "〽")
     (image . "part_alternation_mark.png")
     (style . "github"))
    (:roller-coaster:
     (unicode . "🎢")
     (image . "roller_coaster.png")
     (style . "github"))
    (:flag-gh:
     (unicode . "🇬🇭")
     (image . "flag_gh.png")
     (style . "github"))
    (:knife:
     (unicode . "🔪")
     (image . "knife.png")
     (style . "github"))
    (:om_symbol:
     (unicode . "🕉")
     (image . "om_symbol.png")
     (style . "github"))
    (:no-entry-sign:
     (unicode . "🚫")
     (image . "no_entry_sign.png")
     (style . "github"))
    (:stuck-out-tongue-winking-eye:
     (ascii . ">:P")
     (unicode . "😜")
     (image . "stuck_out_tongue_winking_eye.png")
     (style . "github"))
    (:horse_racing_tone1:
     (unicode . "🏇🏻")
     (image . "horse_racing_tone1.png")
     (style . "github"))
    (:film_frames:
     (unicode . "🎞")
     (image . "film_frames.png")
     (style . "github"))
    (:flag_bg:
     (unicode . "🇧🇬")
     (image . "flag_bg.png")
     (style . "github"))
    (:flag-us:
     (unicode . "🇺🇸")
     (image . "flag_us.png")
     (style . "github"))
    (:flag-hn:
     (unicode . "🇭🇳")
     (image . "flag_hn.png")
     (style . "github"))
    (:flag-yt:
     (unicode . "🇾🇹")
     (image . "flag_yt.png")
     (style . "github"))
    (:military_medal:
     (unicode . "🎖")
     (image . "military_medal.png")
     (style . "github"))
    (:man_tone1:
     (unicode . "👨🏻")
     (image . "man_tone1.png")
     (style . "github"))
    (:notes:
     (unicode . "🎶")
     (image . "notes.png")
     (style . "github"))
    (:writing-hand:
     (unicode . "✍")
     (image . "writing_hand.png")
     (style . "github"))
    (:flag_je:
     (unicode . "🇯🇪")
     (image . "flag_je.png")
     (style . "github"))
    (:punch_tone4:
     (unicode . "👊🏾")
     (image . "punch_tone4.png")
     (style . "github"))
    (:flag-mt:
     (unicode . "🇲🇹")
     (image . "flag_mt.png")
     (style . "github"))
    (:first_quarter_moon_with_face:
     (unicode . "🌛")
     (image . "first_quarter_moon_with_face.png")
     (style . "github"))
    (:runner-tone4:
     (unicode . "🏃🏾")
     (image . "runner_tone4.png")
     (style . "github"))
    (:partly_sunny:
     (unicode . "⛅")
     (image . "partly_sunny.png")
     (style . "github"))
    (:walking-tone2:
     (unicode . "🚶🏼")
     (image . "walking_tone2.png")
     (style . "github"))
    (:flag_pw:
     (unicode . "🇵🇼")
     (image . "flag_pw.png")
     (style . "github"))
    (:flag_ao:
     (unicode . "🇦🇴")
     (image . "flag_ao.png")
     (style . "github"))
    (:mountain_railway:
     (unicode . "🚞")
     (image . "mountain_railway.png")
     (style . "github"))
    (:large_orange_diamond:
     (unicode . "🔶")
     (image . "large_orange_diamond.png")
     (style . "github"))
    (:relaxed:
     (unicode . "☺")
     (image . "relaxed.png")
     (style . "github"))
    (:thunder_cloud_rain:
     (unicode . "⛈")
     (image . "thunder_cloud_rain.png")
     (style . "github"))
    (:chart_with_downwards_trend:
     (unicode . "📉")
     (image . "chart_with_downwards_trend.png")
     (style . "github"))
    (:two_women_holding_hands:
     (unicode . "👭")
     (image . "two_women_holding_hands.png")
     (style . "github"))
    (:heavy-minus-sign:
     (unicode . "➖")
     (image . "heavy_minus_sign.png")
     (style . "github"))
    (:straight-ruler:
     (unicode . "📏")
     (image . "straight_ruler.png")
     (style . "github"))
    (:card_index:
     (unicode . "📇")
     (image . "card_index.png")
     (style . "github"))
    (:flag_tg:
     (unicode . "🇹🇬")
     (image . "flag_tg.png")
     (style . "github"))
    (:train2:
     (unicode . "🚆")
     (image . "train2.png")
     (style . "github"))
    (:pen_ballpoint:
     (unicode . "🖊")
     (image . "pen_ballpoint.png")
     (style . "github"))
    (:baby_tone2:
     (unicode . "👶🏼")
     (image . "baby_tone2.png")
     (style . "github"))
    (:tangerine:
     (unicode . "🍊")
     (image . "tangerine.png")
     (style . "github"))
    (:lifter-tone5:
     (unicode . "🏋🏿")
     (image . "lifter_tone5.png")
     (style . "github"))
    (:peace:
     (unicode . "☮")
     (image . "peace.png")
     (style . "github"))
    (:cat2:
     (unicode . "🐈")
     (image . "cat2.png")
     (style . "github"))
    (:point-left-tone5:
     (unicode . "👈🏿")
     (image . "point_left_tone5.png")
     (style . "github"))
    (:angel_tone2:
     (unicode . "👼🏼")
     (image . "angel_tone2.png")
     (style . "github"))
    (:stuck-out-tongue-closed-eyes:
     (unicode . "😝")
     (image . "stuck_out_tongue_closed_eyes.png")
     (style . "github"))
    (:wave_tone5:
     (unicode . "👋🏿")
     (image . "wave_tone5.png")
     (style . "github"))
    (:place-of-worship:
     (unicode . "🛐")
     (image . "place_of_worship.png")
     (style . "github"))
    (:flag-tc:
     (unicode . "🇹🇨")
     (image . "flag_tc.png")
     (style . "github"))
    (:baby-tone5:
     (unicode . "👶🏿")
     (image . "baby_tone5.png")
     (style . "github"))
    (:left_right_arrow:
     (unicode . "↔")
     (image . "left_right_arrow.png")
     (style . "github"))
    (:three:
     (unicode . "3⃣")
     (image . "three.png")
     (style . "github"))
    (:middle_finger_tone3:
     (unicode . "🖕🏽")
     (image . "middle_finger_tone3.png")
     (style . "github"))
    (:flag-cf:
     (unicode . "🇨🇫")
     (image . "flag_cf.png")
     (style . "github"))
    (:flag_co:
     (unicode . "🇨🇴")
     (image . "flag_co.png")
     (style . "github"))
    (:flag-it:
     (unicode . "🇮🇹")
     (image . "flag_it.png")
     (style . "github"))
    (:thumbsup_tone2:
     (unicode . "👍🏼")
     (image . "thumbsup_tone2.png")
     (style . "github"))
    (:camel:
     (unicode . "🐫")
     (image . "camel.png")
     (style . "github"))
    (:panda_face:
     (unicode . "🐼")
     (image . "panda_face.png")
     (style . "github"))
    (:clock10:
     (unicode . "🕙")
     (image . "clock10.png")
     (style . "github"))
    (:prayer_beads:
     (unicode . "📿")
     (image . "prayer_beads.png")
     (style . "github"))
    (:guitar:
     (unicode . "🎸")
     (image . "guitar.png")
     (style . "github"))
    (:chipmunk:
     (unicode . "🐿")
     (image . "chipmunk.png")
     (style . "github"))
    (:tc:
     (unicode . "🇹🇨")
     (image . "flag_tc.png")
     (style . "github"))
    (:dividers:
     (unicode . "🗂")
     (image . "dividers.png")
     (style . "github"))
    (:family_mmgb:
     (unicode . "👨👨👧👦")
     (image . "family_mmgb.png")
     (style . "github"))
    (:family_mwbb:
     (unicode . "👨👩👦👦")
     (image . "family_mwbb.png")
     (style . "github"))
    (:high_brightness:
     (unicode . "🔆")
     (image . "high_brightness.png")
     (style . "github"))
    (:cheese_wedge:
     (unicode . "🧀")
     (image . "cheese.png")
     (style . "github"))
    (:flag_xk:
     (unicode . "🇽🇰")
     (image . "flag_xk.png")
     (style . "github"))
    (:flag_ru:
     (unicode . "🇷🇺")
     (image . "flag_ru.png")
     (style . "github"))
    (:flag_cy:
     (unicode . "🇨🇾")
     (image . "flag_cy.png")
     (style . "github"))
    (:ok_woman_tone2:
     (unicode . "🙆🏼")
     (image . "ok_woman_tone2.png")
     (style . "github"))
    (:bn:
     (unicode . "🇧🇳")
     (image . "flag_bn.png")
     (style . "github"))
    (:family_mwgb:
     (unicode . "👨👩👧👦")
     (image . "family_mwgb.png")
     (style . "github"))
    (:point-up-tone5:
     (unicode . "☝🏿")
     (image . "point_up_tone5.png")
     (style . "github"))
    (:flag_py:
     (unicode . "🇵🇾")
     (image . "flag_py.png")
     (style . "github"))
    (:flag_wf:
     (unicode . "🇼🇫")
     (image . "flag_wf.png")
     (style . "github"))
    (:flag_ge:
     (unicode . "🇬🇪")
     (image . "flag_ge.png")
     (style . "github"))
    (:flag_kw:
     (unicode . "🇰🇼")
     (image . "flag_kw.png")
     (style . "github"))
    (:snowman2:
     (unicode . "☃")
     (image . "snowman2.png")
     (style . "github"))
    (:metal-tone2:
     (unicode . "🤘🏼")
     (image . "metal_tone2.png")
     (style . "github"))
    (:sm:
     (unicode . "🇸🇲")
     (image . "flag_sm.png")
     (style . "github"))
    (:flag_ve:
     (unicode . "🇻🇪")
     (image . "flag_ve.png")
     (style . "github"))
    (:nail-care-tone4:
     (unicode . "💅🏾")
     (image . "nail_care_tone4.png")
     (style . "github"))
    (:studio_microphone:
     (unicode . "🎙")
     (image . "microphone2.png")
     (style . "github"))
    (:raised_hand_with_part_between_middle_and_ring_fingers_tone4:
     (unicode . "🖖🏾")
     (image . "vulcan_tone4.png")
     (style . "github"))
    (:arrows_counterclockwise:
     (unicode . "🔄")
     (image . "arrows_counterclockwise.png")
     (style . "github"))
    (:wheel_of_dharma:
     (unicode . "☸")
     (image . "wheel_of_dharma.png")
     (style . "github"))
    (:reversed_hand_with_middle_finger_extended_tone4:
     (unicode . "🖕🏾")
     (image . "middle_finger_tone4.png")
     (style . "github"))
    (:-1_tone1:
     (unicode . "👎🏻")
     (image . "thumbsdown_tone1.png")
     (style . "github"))
    (:sports_medal:
     (unicode . "🏅")
     (image . "medal.png")
     (style . "github"))
    (:unamused:
     (unicode . "😒")
     (image . "unamused.png")
     (style . "github"))
    (:see_no_evil:
     (unicode . "🙈")
     (image . "see_no_evil.png")
     (style . "github"))
    (:construction_worker_tone2:
     (unicode . "👷🏼")
     (image . "construction_worker_tone2.png")
     (style . "github"))
    (:shower:
     (unicode . "🚿")
     (image . "shower.png")
     (style . "github"))
    (:basketball-player-tone5:
     (unicode . "⛹🏿")
     (image . "basketball_player_tone5.png")
     (style . "github"))
    (:sleuth_or_spy_tone4:
     (unicode . "🕵🏾")
     (image . "spy_tone4.png")
     (style . "github"))
    (:information_source:
     (unicode . "ℹ")
     (image . "information_source.png")
     (style . "github"))
    (:cool:
     (unicode . "🆒")
     (image . "cool.png")
     (style . "github"))
    (:bee:
     (unicode . "🐝")
     (image . "bee.png")
     (style . "github"))
    (:closed-umbrella:
     (unicode . "🌂")
     (image . "closed_umbrella.png")
     (style . "github"))
    (:eight:
     (unicode . "8⃣")
     (image . "eight.png")
     (style . "github"))
    (:family_mwg:
     (unicode . "👨👩👧")
     (image . "family_mwg.png")
     (style . "github"))
    (:flag-py:
     (unicode . "🇵🇾")
     (image . "flag_py.png")
     (style . "github"))
    (:nose_tone2:
     (unicode . "👃🏼")
     (image . "nose_tone2.png")
     (style . "github"))
    (:rowboat_tone1:
     (unicode . "🚣🏻")
     (image . "rowboat_tone1.png")
     (style . "github"))
    (:rs:
     (unicode . "🇷🇸")
     (image . "flag_rs.png")
     (style . "github"))
    (:curly_loop:
     (unicode . "➰")
     (image . "curly_loop.png")
     (style . "github"))
    (:slight_smile:
     (unicode . "🙂")
     (image . "slight_smile.png")
     (style . "github"))
    (:flag-xk:
     (unicode . "🇽🇰")
     (image . "flag_xk.png")
     (style . "github"))
    (:punch-tone1:
     (unicode . "👊🏻")
     (image . "punch_tone1.png")
     (style . "github"))
    (:field_hockey:
     (unicode . "🏑")
     (image . "field_hockey.png")
     (style . "github"))
    (:point_down_tone2:
     (unicode . "👇🏼")
     (image . "point_down_tone2.png")
     (style . "github"))
    (:heart-decoration:
     (unicode . "💟")
     (image . "heart_decoration.png")
     (style . "github"))
    (:flag_sy:
     (unicode . "🇸🇾")
     (image . "flag_sy.png")
     (style . "github"))
    (:family:
     (unicode . "👪")
     (image . "family.png")
     (style . "github"))
    (:oil:
     (unicode . "🛢")
     (image . "oil.png")
     (style . "github"))
    (:white_frowning_face:
     (unicode . "☹")
     (image . "frowning2.png")
     (style . "github"))
    (:bar-chart:
     (unicode . "📊")
     (image . "bar_chart.png")
     (style . "github"))
    (:congratulations:
     (unicode . "㊗")
     (image . "congratulations.png")
     (style . "github"))
    (:flag-nc:
     (unicode . "🇳🇨")
     (image . "flag_nc.png")
     (style . "github"))
    (:pray:
     (unicode . "🙏")
     (image . "pray.png")
     (style . "github"))
    (:flag_nu:
     (unicode . "🇳🇺")
     (image . "flag_nu.png")
     (style . "github"))
    (:fj:
     (unicode . "🇫🇯")
     (image . "flag_fj.png")
     (style . "github"))
    (:open-hands-tone2:
     (unicode . "👐🏼")
     (image . "open_hands_tone2.png")
     (style . "github"))
    (:kiss-mm:
     (unicode . "👨❤💋👨")
     (image . "kiss_mm.png")
     (style . "github"))
    (:spiral_calendar_pad:
     (unicode . "🗓")
     (image . "calendar_spiral.png")
     (style . "github"))
    (:congo:
     (unicode . "🇨🇩")
     (image . "flag_cd.png")
     (style . "github"))
    (:fast_forward:
     (unicode . "⏩")
     (image . "fast_forward.png")
     (style . "github"))
    (:label:
     (unicode . "🏷")
     (image . "label.png")
     (style . "github"))
    (:red_car:
     (unicode . "🚗")
     (image . "red_car.png")
     (style . "github"))
    (:raised-hands-tone3:
     (unicode . "🙌🏽")
     (image . "raised_hands_tone3.png")
     (style . "github"))
    (:yum:
     (unicode . "😋")
     (image . "yum.png")
     (style . "github"))
    (:guardsman_tone5:
     (unicode . "💂🏿")
     (image . "guardsman_tone5.png")
     (style . "github"))
    (:information_desk_person_tone1:
     (unicode . "💁🏻")
     (image . "information_desk_person_tone1.png")
     (style . "github"))
    (:bottle_with_popping_cork:
     (unicode . "🍾")
     (image . "champagne.png")
     (style . "github"))
    (:white_sun_behind_cloud_with_rain:
     (unicode . "🌦")
     (image . "white_sun_rain_cloud.png")
     (style . "github"))
    (:flag_kh:
     (unicode . "🇰🇭")
     (image . "flag_kh.png")
     (style . "github"))
    (:symbols:
     (unicode . "🔣")
     (image . "symbols.png")
     (style . "github"))
    (:face_with_head_bandage:
     (unicode . "🤕")
     (image . "head_bandage.png")
     (style . "github"))
    (:spy-tone5:
     (unicode . "🕵🏿")
     (image . "spy_tone5.png")
     (style . "github"))
    (:mailbox_with_mail:
     (unicode . "📬")
     (image . "mailbox_with_mail.png")
     (style . "github"))
    (:flashlight:
     (unicode . "🔦")
     (image . "flashlight.png")
     (style . "github"))
    (:pencil:
     (unicode . "📝")
     (image . "pencil.png")
     (style . "github"))
    (:raised_hands_tone3:
     (unicode . "🙌🏽")
     (image . "raised_hands_tone3.png")
     (style . "github"))
    (:question:
     (unicode . "❓")
     (image . "question.png")
     (style . "github"))
    (:leo:
     (unicode . "♌")
     (image . "leo.png")
     (style . "github"))
    (:thought_balloon:
     (unicode . "💭")
     (image . "thought_balloon.png")
     (style . "github"))
    (:womans-hat:
     (unicode . "👒")
     (image . "womans_hat.png")
     (style . "github"))
    (:man_tone2:
     (unicode . "👨🏼")
     (image . "man_tone2.png")
     (style . "github"))
    (:dizzy-face:
     (ascii . "#-)")
     (unicode . "😵")
     (image . "dizzy_face.png")
     (style . "github"))
    (:scream-cat:
     (unicode . "🙀")
     (image . "scream_cat.png")
     (style . "github"))
    (:full-moon-with-face:
     (unicode . "🌝")
     (image . "full_moon_with_face.png")
     (style . "github"))
    (:wind_blowing_face:
     (unicode . "🌬")
     (image . "wind_blowing_face.png")
     (style . "github"))
    (:hammer_and_pick:
     (unicode . "⚒")
     (image . "hammer_pick.png")
     (style . "github"))
    (:flag_qa:
     (unicode . "🇶🇦")
     (image . "flag_qa.png")
     (style . "github"))
    (:raised_hand_with_fingers_splayed_tone1:
     (unicode . "🖐🏻")
     (image . "hand_splayed_tone1.png")
     (style . "github"))
    (:nose-tone3:
     (unicode . "👃🏽")
     (image . "nose_tone3.png")
     (style . "github"))
    (:flag-ye:
     (unicode . "🇾🇪")
     (image . "flag_ye.png")
     (style . "github"))
    (:pause-button:
     (unicode . "⏸")
     (image . "pause_button.png")
     (style . "github"))
    (:light_rail:
     (unicode . "🚈")
     (image . "light_rail.png")
     (style . "github"))
    (:dart:
     (unicode . "🎯")
     (image . "dart.png")
     (style . "github"))
    (:flag-sl:
     (unicode . "🇸🇱")
     (image . "flag_sl.png")
     (style . "github"))
    (:crescent_moon:
     (unicode . "🌙")
     (image . "crescent_moon.png")
     (style . "github"))
    (:flag-bv:
     (unicode . "🇧🇻")
     (image . "flag_bv.png")
     (style . "github"))
    (:palm_tree:
     (unicode . "🌴")
     (image . "palm_tree.png")
     (style . "github"))
    (:massage_tone2:
     (unicode . "💆🏼")
     (image . "massage_tone2.png")
     (style . "github"))
    (:er:
     (unicode . "🇪🇷")
     (image . "flag_er.png")
     (style . "github"))
    (:thumbsdown_tone5:
     (unicode . "👎🏿")
     (image . "thumbsdown_tone5.png")
     (style . "github"))
    (:flag-bo:
     (unicode . "🇧🇴")
     (image . "flag_bo.png")
     (style . "github"))
    (:point-up-tone2:
     (unicode . "☝🏼")
     (image . "point_up_tone2.png")
     (style . "github"))
    (:flag_gp:
     (unicode . "🇬🇵")
     (image . "flag_gp.png")
     (style . "github"))
    (:person_frowning_tone2:
     (unicode . "🙍🏼")
     (image . "person_frowning_tone2.png")
     (style . "github"))
    (:grandma_tone1:
     (unicode . "👵🏻")
     (image . "older_woman_tone1.png")
     (style . "github"))
    (:mp:
     (unicode . "🇲🇵")
     (image . "flag_mp.png")
     (style . "github"))
    (:flag-la:
     (unicode . "🇱🇦")
     (image . "flag_la.png")
     (style . "github"))
    (:flag-do:
     (unicode . "🇩🇴")
     (image . "flag_do.png")
     (style . "github"))
    (:thumbsup-tone5:
     (unicode . "👍🏿")
     (image . "thumbsup_tone5.png")
     (style . "github"))
    (:flag-bj:
     (unicode . "🇧🇯")
     (image . "flag_bj.png")
     (style . "github"))
    (:microscope:
     (unicode . "🔬")
     (image . "microscope.png")
     (style . "github"))
    (:flag-fo:
     (unicode . "🇫🇴")
     (image . "flag_fo.png")
     (style . "github"))
    (:couplekiss_mm:
     (unicode . "👨❤💋👨")
     (image . "kiss_mm.png")
     (style . "github"))
    (:ophiuchus:
     (unicode . "⛎")
     (image . "ophiuchus.png")
     (style . "github"))
    (:man-with-gua-pi-mao-tone3:
     (unicode . "👲🏽")
     (image . "man_with_gua_pi_mao_tone3.png")
     (style . "github"))
    (:flag_sk:
     (unicode . "🇸🇰")
     (image . "flag_sk.png")
     (style . "github"))))

(defvar eslack--emoji-hash
  (let ((hash (make-hash-table)))
    (cl-loop for (key . props) in eslack--emoji-alist
             do (puthash key props hash))
    hash))

(defun eslack--emoji-ids ()
  (mapcar (lambda (entry)
            (symbol-name (car entry)))
          eslack--emoji-alist ))

(provide 'eslack-emoji)
;;; eslack-emoji.el ends here
