/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.util.color

/** Raw data adapted from perl's Term-ExtendedColor, which is published
 *  under perl's Artistic license: http://dev.perl.org/licenses/artistic.html
 *
 *  These aren't actually in use yet.
 */
abstract class ColorNames {
  type ColorType
  def translateCode(ansiCode: String): ColorType

  @inline private implicit def liftAnsiCode(code: String): ColorType = translateCode(code)

  // Possible alternative names or aliases, also from the perl:
  //
  // reset, clear, normal        reset all attributes
  // bold, bright                bold or bright, depending on implementation
  // faint                       decreased intensity (not widely supported)
  // italic, cursive             italic or cursive
  // underline, underscore       underline
  // blink                       slow blink
  // blink_ms                    rapid blink (only supported in MS DOS)
  // reverse, inverse, negative  reverse video
  // conceal                     conceal, or hide (not widely supported)

  // Brightest to darkest color
  val red1: ColorType = "5;196"
  val red2: ColorType = "5;160"
  val red3: ColorType = "5;124"
  val red4: ColorType = "5;088"
  val red5: ColorType = "5;052"

  val green1: ColorType = "5;156"
  val green2: ColorType = "5;150"
  val green3: ColorType = "5;120"
  val green4: ColorType = "5;114"
  val green5: ColorType = "5;084"
  val green6: ColorType = "5;078"
  val green7: ColorType = "5;155"
  val green8: ColorType = "5;149"
  val green9: ColorType = "5;119"
  val green10: ColorType = "5;113"
  val green11: ColorType = "5;083"
  val green12: ColorType = "5;077"
  val green13: ColorType = "5;047"
  val green14: ColorType = "5;041"
  val green15: ColorType = "5;118"
  val green16: ColorType = "5;112"
  val green17: ColorType = "5;082"
  val green18: ColorType = "5;076"
  val green19: ColorType = "5;046"
  val green20: ColorType = "5;040"
  val green21: ColorType = "5;034"
  val green22: ColorType = "5;028"
  val green23: ColorType = "5;022"
  val green24: ColorType = "5;107"
  val green25: ColorType = "5;071"
  val green26: ColorType = "5;070"
  val green27: ColorType = "5;064"
  val green28: ColorType = "5;065"

  val blue1: ColorType = "5;075"
  val blue2: ColorType = "5;074"
  val blue3: ColorType = "5;073"
  val blue4: ColorType = "5;039"
  val blue5: ColorType = "5;038"
  val blue6: ColorType = "5;037"
  val blue7: ColorType = "5;033"
  val blue8: ColorType = "5;032"
  val blue9: ColorType = "5;031"
  val blue10: ColorType = "5;027"
  val blue11: ColorType = "5;026"
  val blue12: ColorType = "5;025"
  val blue13: ColorType = "5;021"
  val blue14: ColorType = "5;020"
  val blue15: ColorType = "5;019"
  val blue16: ColorType = "5;018"
  val blue17: ColorType = "5;017"

  val yellow1: ColorType = "5;228"
  val yellow2: ColorType = "5;222"
  val yellow3: ColorType = "5;192"
  val yellow4: ColorType = "5;186"
  val yellow5: ColorType = "5;227"
  val yellow6: ColorType = "5;221"
  val yellow7: ColorType = "5;191"
  val yellow8: ColorType = "5;185"
  val yellow9: ColorType = "5;226"
  val yellow10: ColorType = "5;220"
  val yellow11: ColorType = "5;190"
  val yellow12: ColorType = "5;184"
  val yellow13: ColorType = "5;214"
  val yellow14: ColorType = "5;178"
  val yellow15: ColorType = "5;208"
  val yellow16: ColorType = "5;172"
  val yellow17: ColorType = "5;202"
  val yellow18: ColorType = "5;166"

  val magenta1: ColorType  = "5;219"
  val magenta2: ColorType  = "5;183"
  val magenta3: ColorType  = "5;218"
  val magenta4: ColorType  = "5;182"
  val magenta5: ColorType  = "5;217"
  val magenta6: ColorType  = "5;181"
  val magenta7: ColorType  = "5;213"
  val magenta8: ColorType  = "5;177"
  val magenta9: ColorType  = "5;212"
  val magenta10: ColorType = "5;176"
  val magenta11: ColorType = "5;211"
  val magenta12: ColorType = "5;175"
  val magenta13: ColorType = "5;207"
  val magenta14: ColorType = "5;171"
  val magenta15: ColorType = "5;205"
  val magenta16: ColorType = "5;169"
  val magenta17: ColorType = "5;201"
  val magenta18: ColorType = "5;165"
  val magenta19: ColorType = "5;200"
  val magenta20: ColorType = "5;164"
  val magenta21: ColorType = "5;199"
  val magenta22: ColorType = "5;163"
  val magenta23: ColorType = "5;198"
  val magenta24: ColorType = "5;162"
  val magenta25: ColorType = "5;197"
  val magenta26: ColorType = "5;161"

  val gray1: ColorType = "5;255"
  val gray2: ColorType = "5;254"
  val gray3: ColorType = "5;253"
  val gray4: ColorType = "5;252"
  val gray5: ColorType = "5;251"
  val gray6: ColorType = "5;250"
  val gray7: ColorType = "5;249"
  val gray8: ColorType = "5;248"
  val gray9: ColorType = "5;247"
  val gray10: ColorType = "5;246"
  val gray11: ColorType = "5;245"
  val gray12: ColorType = "5;244"
  val gray13: ColorType = "5;243"
  val gray14: ColorType = "5;242"
  val gray15: ColorType = "5;241"
  val gray16: ColorType = "5;240"
  val gray17: ColorType = "5;239"
  val gray18: ColorType = "5;238"
  val gray19: ColorType = "5;237"
  val gray20: ColorType = "5;236"
  val gray21: ColorType = "5;235"
  val gray22: ColorType = "5;234"
  val gray23: ColorType = "5;233"
  val gray24: ColorType = "5;232"

  val purple1: ColorType = "5;147"
  val purple2: ColorType = "5;146"
  val purple3: ColorType = "5;145"
  val purple4: ColorType = "5;141"
  val purple5: ColorType = "5;140"
  val purple6: ColorType = "5;139"
  val purple7: ColorType = "5;135"
  val purple8: ColorType = "5;134"
  val purple9: ColorType = "5;133"
  val purple10: ColorType = "5;129"
  val purple11: ColorType = "5;128"
  val purple12: ColorType = "5;127"
  val purple13: ColorType = "5;126"
  val purple14: ColorType = "5;125"
  val purple15: ColorType = "5;111"
  val purple16: ColorType = "5;110"
  val purple17: ColorType = "5;109"
  val purple18: ColorType = "5;105"
  val purple19: ColorType = "5;104"
  val purple20: ColorType = "5;103"
  val purple21: ColorType = "5;099"
  val purple22: ColorType = "5;098"
  val purple23: ColorType = "5;097"
  val purple24: ColorType = "5;096"
  val purple25: ColorType = "5;093"
  val purple26: ColorType = "5;092"
  val purple27: ColorType = "5;091"
  val purple28: ColorType = "5;090"
  val purple29: ColorType = "5;055"
  val purple30: ColorType = "5;054"

  val cyan1: ColorType = "5;159"
  val cyan2: ColorType = "5;158"
  val cyan3: ColorType = "5;157"
  val cyan4: ColorType = "5;153"
  val cyan5: ColorType = "5;152"
  val cyan6: ColorType = "5;151"
  val cyan7: ColorType = "5;123"
  val cyan8: ColorType = "5;122"
  val cyan9: ColorType = "5;121"
  val cyan10: ColorType = "5;117"
  val cyan11: ColorType = "5;116"
  val cyan12: ColorType = "5;115"
  val cyan13: ColorType = "5;087"
  val cyan14: ColorType = "5;086"
  val cyan15: ColorType = "5;085"
  val cyan16: ColorType = "5;081"
  val cyan17: ColorType = "5;080"
  val cyan18: ColorType = "5;079"
  val cyan19: ColorType = "5;051"
  val cyan20: ColorType = "5;050"
  val cyan21: ColorType = "5;049"
  val cyan22: ColorType = "5;045"
  val cyan23: ColorType = "5;044"
  val cyan24: ColorType = "5;043"

  val orange1: ColorType = "5;208"
  val orange2: ColorType = "5;172"
  val orange3: ColorType = "5;202"
  val orange4: ColorType = "5;166"
  val orange5: ColorType = "5;130"

  // Approximations of X11 color mappings
  // https://secure.wikimedia.org/wikipedia/en/wiki/X11%20colors

  val aquamarine1: ColorType = "5;086"
  val aquamarine3: ColorType = "5;079"
  val blueviolet: ColorType = "5;057"
  val cadetblue1: ColorType = "5;072"
  val cadetblue2: ColorType = "5;073"
  val chartreuse1: ColorType = "5;118"
  val chartreuse2: ColorType = "5;082"
  val chartreuse3: ColorType = "5;070"
  val chartreuse4: ColorType = "5;064"
  val cornflowerblue: ColorType = "5;069"
  val cornsilk1: ColorType = "5;230"
  val darkblue: ColorType = "5;018"
  val darkcyan: ColorType = "5;036"
  val darkgoldenrod: ColorType = "5;136"
  val darkgreen: ColorType = "5;022"
  val darkkhaki: ColorType = "5;143"
  val darkmagenta1: ColorType = "5;090"
  val darkmagenta2: ColorType = "5;091"
  val darkolivegreen1: ColorType = "5;191"
  val darkolivegreen2: ColorType = "5;155"
  val darkolivegreen3: ColorType = "5;107"
  val darkolivegreen4: ColorType = "5;113"
  val darkolivegreen5: ColorType = "5;149"
  val darkorange3: ColorType = "5;130"
  val darkorange4: ColorType = "5;166"
  val darkorange1: ColorType = "5;208"
  val darkred1: ColorType = "5;052"
  val darkred2: ColorType = "5;088"
  val darkseagreen1: ColorType = "5;158"
  val darkseagreen2: ColorType = "5;157"
  val darkseagreen3: ColorType = "5;150"
  val darkseagreen4: ColorType = "5;071"
  val darkslategray1: ColorType = "5;123"
  val darkslategray2: ColorType = "5;087"
  val darkslategray3: ColorType = "5;116"
  val darkturquoise: ColorType = "5;044"
  val darkviolet: ColorType = "5;128"
  val deeppink1: ColorType = "5;198"
  val deeppink2: ColorType = "5;197"
  val deeppink3: ColorType = "5;162"
  val deeppink4: ColorType = "5;125"
  val deepskyblue1: ColorType = "5;039"
  val deepskyblue2: ColorType = "5;038"
  val deepskyblue3: ColorType = "5;031"
  val deepskyblue4: ColorType = "5;023"
  val dodgerblue1: ColorType = "5;033"
  val dodgerblue2: ColorType = "5;027"
  val dodgerblue3: ColorType = "5;026"
  val gold1: ColorType = "5;220"
  val gold3: ColorType = "5;142"
  val greenyellow: ColorType = "5;154"
  val grey0: ColorType = "5;016"
  val grey100: ColorType = "5;231"
  val grey11: ColorType = "5;234"
  val grey15: ColorType = "5;235"
  val grey19: ColorType = "5;236"
  val grey23: ColorType = "5;237"
  val grey27: ColorType = "5;238"
  val grey30: ColorType = "5;239"
  val grey3: ColorType = "5;232"
  val grey35: ColorType = "5;240"
  val grey37: ColorType = "5;059"
  val grey39: ColorType = "5;241"
  val grey42: ColorType = "5;242"
  val grey46: ColorType = "5;243"
  val grey50: ColorType = "5;244"
  val grey53: ColorType = "5;102"
  val grey54: ColorType = "5;245"
  val grey58: ColorType = "5;246"
  val grey62: ColorType = "5;247"
  val grey63: ColorType = "5;139"
  val grey66: ColorType = "5;248"
  val grey69: ColorType = "5;145"
  val grey70: ColorType = "5;249"
  val grey74: ColorType = "5;250"
  val grey7: ColorType = "5;233"
  val grey78: ColorType = "5;251"
  val grey82: ColorType = "5;252"
  val grey84: ColorType = "5;188"
  val grey85: ColorType = "5;253"
  val grey89: ColorType = "5;254"
  val grey93: ColorType = "5;255"
  val honeydew2: ColorType = "5;194"
  val hotpink2: ColorType = "5;169"
  val hotpink3: ColorType = "5;132"
  val hotpink: ColorType = "5;205"
  val indianred1: ColorType = "5;203"
  val indianred: ColorType = "5;167"
  val khaki1: ColorType = "5;228"
  val khaki3: ColorType = "5;185"
  val lightcoral: ColorType = "5;210"
  val lightcyan1: ColorType = "5;195"
  val lightcyan3: ColorType = "5;152"
  val lightgoldenrod1: ColorType = "5;227"
  val lightgoldenrod2: ColorType = "5;186"
  val lightgoldenrod3: ColorType = "5;179"
  val lightgreen: ColorType = "5;119"
  val lightpink1: ColorType = "5;217"
  val lightpink3: ColorType = "5;174"
  val lightpink4: ColorType = "5;095"
  val lightsalmon1: ColorType = "5;216"
  val lightsalmon3: ColorType = "5;137"
  val lightseagreen: ColorType = "5;037"
  val lightskyblue1: ColorType = "5;153"
  val lightskyblue3: ColorType = "5;109"
  val lightslateblue: ColorType = "5;105"
  val lightslategrey: ColorType = "5;103"
  val lightsteelblue1: ColorType = "5;189"
  val lightsteelblue3: ColorType = "5;146"
  val lightsteelblue: ColorType = "5;147"
  val lightyellow3: ColorType = "5;187"
  val mediumorchid1: ColorType = "5;171"
  val mediumorchid3: ColorType = "5;133"
  val mediumorchid: ColorType = "5;134"
  val mediumpurple1: ColorType = "5;141"
  val mediumpurple2: ColorType = "5;135"
  val mediumpurple3: ColorType = "5;097"
  val mediumpurple4: ColorType = "5;060"
  val mediumpurple: ColorType = "5;104"
  val mediumspringgreen: ColorType = "5;049"
  val mediumturquoise: ColorType = "5;080"
  val mediumvioletred: ColorType = "5;126"
  val mistyrose1: ColorType = "5;224"
  val mistyrose3: ColorType = "5;181"
  val navajowhite1: ColorType = "5;223"
  val navajowhite3: ColorType = "5;144"
  val navyblue: ColorType = "5;017"
  val orangered1: ColorType = "5;202"
  val orchid1: ColorType = "5;213"
  val orchid2: ColorType = "5;212"
  val orchid: ColorType = "5;170"
  val palegreen1: ColorType = "5;121"
  val palegreen3: ColorType = "5;077"
  val paleturquoise1: ColorType = "5;159"
  val paleturquoise4: ColorType = "5;066"
  val palevioletred1: ColorType = "5;211"
  val pink1: ColorType = "5;218"
  val pink3: ColorType = "5;175"
  val plum1: ColorType = "5;219"
  val plum2: ColorType = "5;183"
  val plum3: ColorType = "5;176"
  val plum4: ColorType = "5;096"
  val purple: ColorType = "5;129"
  val rosybrown: ColorType = "5;138"
  val royalblue1: ColorType = "5;063"
  val salmon1: ColorType = "5;209"
  val sandybrown: ColorType = "5;215"
  val seagreen1: ColorType = "5;084"
  val seagreen2: ColorType = "5;083"
  val seagreen3: ColorType = "5;078"
  val skyblue1: ColorType = "5;117"
  val skyblue2: ColorType = "5;111"
  val skyblue3: ColorType = "5;074"
  val slateblue1: ColorType = "5;099"
  val slateblue3: ColorType = "5;061"
  val springgreen1: ColorType = "5;048"
  val springgreen2: ColorType = "5;042"
  val springgreen3: ColorType = "5;035"
  val springgreen4: ColorType = "5;029"
  val steelblue1: ColorType = "5;075"
  val steelblue3: ColorType = "5;068"
  val steelblue: ColorType = "5;067"
  val tan: ColorType = "5;180"
  val thistle1: ColorType = "5;225"
  val thistle3: ColorType = "5;182"
  val turquoise2: ColorType = "5;045"
  val turquoise4: ColorType = "5;030"
  val violet: ColorType = "5;177"
  val wheat1: ColorType = "5;229"
  val wheat4: ColorType = "5;101"
}
