package org.cvogt.ansi.colors
object `package`{
  def apply(codes: Int*)(s: String) = codes.map(code).mkString ++ reset(0)(s)
  def applyForeground(codes: Int*)(s: String) = codes.map(code).mkString ++ resetForeground(s)
  def applyBackground(codes: Int*)(s: String) = codes.map(code).mkString ++ resetBackground(s)
  
  private def reset(i: Int)(s: String) = s ++ (if(s endsWith code(i)) "" else code(i))
  
  /** strip ansi color codes */
  def strip(s: String) = s.replaceAll("\u001B\\[[;\\d]*m", "")

  ///** Reset / Normal  all attributes off */
  //def reset(s: String) = apply(0)(s)
  
  /** Bold or increased intensity  */
  def bold(s: String) = code(1) ++ reset(0)(s)
  
  /** Faint (decreased intensity) Not widely supported. */
  def faint(s: String) = apply(2)(s)
  
  /** Italic: on  Not widely supported. Sometimes treated as inverse. */
  def italic(s: String) = apply(3)(s)
  
  /** Underline: Single  */
  def underline(s: String) = code(4) ++ resetUnderline(s)
  
  /** Blink: Slow less than 150 per minute */
  def blinkSlow(s: String) = apply(5)(s)
  
  /** Blink: Rapid  MS-DOS ANSI.SYS; 150+ per minute; not widely supported */
  def blinkRapid(s: String) = apply(6)(s)
  
  /** Image: Negative inverse or reverse; swap foreground and background (reverse video) */
  def image(s: String) = apply(7)(s)
  
  /** Conceal Not widely supported. */
  def concealed(s: String) = apply(8)(s)
  
  /** Crossed-out Characters legible, but marked for deletion. Not widely supported. */
  def crossed(s: String) = apply(9)(s)
  
  /** Primary(default) font  */
  def primary(s: String) = apply(10)(s)
  
  /** alternative font 1 */
  def font1(s: String) = apply(11)(s)
  
  /** alternative font 2 */
  def font2(s: String) = apply(12)(s)
  
  /** alternative font 3 */
  def font3(s: String) = apply(13)(s)
  
  /** alternative font 4 */
  def font4(s: String) = apply(14)(s)
  
  /** alternative font 5 */
  def font5(s: String) = apply(15)(s)
  
  /** alternative font 6 */
  def font6(s: String) = apply(16)(s)
  
  /** alternative font 7 */
  def font7(s: String) = apply(17)(s)
  
  /** alternative font 8 */
  def font8(s: String) = apply(18)(s)
  
  /** alternative font 9 */
  def font9(s: String) = apply(19)(s)
  
  /** Fraktur hardly ever supported */
  def fraktur(s: String) = apply(20)(s)
  
  /** Bold: off or Underline: Double  Bold off not widely supported; double underline hardly ever supported. */
  private def resetBold(s: String) = reset(21)(s)
  
  /** Normal color or intensity Neither bold nor faint */
  def normal(s: String) = apply(22)(s)
  
  /** Not italic, not Fraktur  */
  def not(s: String) = apply(23)(s)
  
  /** Underline: None Not singly or doubly underlined */
  private def resetUnderline(s: String) = reset(24)(s)
  
  /* 
  /** Blink: off   */
  def blinkOff(s: String) = apply(25)(s)
  
  /** Reserved   */
  def reserved(s: String) = apply(26)(s)
  
  /** Image: Positive  */
  def image(s: String) = apply(27)(s)
  
  /** Reveal  conceal off */
  def reveal(s: String) = apply(28)(s)
  
  /** Not crossed out  */
  def not(s: String) = apply(29)(s)
  */
  
  object foreground{
    private val offset = 30
    /** Set text color */
    def black(s: String) = applyForeground(offset+0)(s)
    /** Set text color */
    def red(s: String) = applyForeground(offset+1)(s)
    /** Set text color */
    def green(s: String) = applyForeground(offset+2)(s)
    /** Set text color */
    def yellow(s: String) = applyForeground(offset+3)(s)
    /** Set text color */
    def blue(s: String) = applyForeground(offset+4)(s)
    /** Set text color */
    def magenta(s: String) = applyForeground(offset+5)(s)
    /** Set text color */
    def cyan(s: String) = applyForeground(offset+6)(s)
    /** Set text color */
    def white(s: String) = applyForeground(offset+7)(s)
  }

  /*
  /** Reserved for extended set foreground color  typical supported next arguments are 5;x where x is color index (0..255) or 2;r;g;b where r,g,b are red, green and blue color channels (out of 255) */
  def reserved(s: String) = apply(38)(s)
  */
  
  /** Default text color (foreground) implementation defined (according to standard) */
  private def resetForeground(s: String) = reset(39)(s)
  
  object background{
    val offset = 40
    /** Set background color */
    def black(s: String) = applyBackground(offset+0)(s)
    /** Set background color */
    def red(s: String) = applyBackground(offset+1)(s)
    /** Set background color */
    def green(s: String) = applyBackground(offset+2)(s)
    /** Set background color */
    def yellow(s: String) = applyBackground(offset+3)(s)
    /** Set background color */
    def blue(s: String) = applyBackground(offset+4)(s)
    /** Set background color */
    def magenta(s: String) = applyBackground(offset+5)(s)
    /** Set background color */
    def cyan(s: String) = applyBackground(offset+6)(s)
    /** Set background color */
    def white(s: String) = applyBackground(offset+7)(s)
  }
       
  /*
  /** Reserved for extended set background color  typical supported next arguments are 5;x where x is color index (0..255) or 2;r;g;b where r,g,b are red, green and blue color channels (out of 255) */
  def reserved(s: String) = apply(48)(s)
  */

  /** Default background color  implementation defined (according to standard) */
  private def resetBackground(s: String) = reset(49)(s)
  
  /*
  /** Reserved   */
  def reserved(s: String) = apply(50)(s)
  
  /** Framed   */
  def framed(s: String) = apply(51)(s)
  */

  /** Encircled  */
  def encircled(s: String) = apply(52)(s)
  
  /** Overlined  */
  def overlined(s: String) = apply(53)(s)
  
  /*
  /** Not framed or encircled  */
  def not(s: String) = apply(54)(s)
  
  /** Not overlined  */
  def not(s: String) = apply(55)(s)
  
  /** Reserved   */
  def reserved(s: String) = apply(56–59)(s)

  /** ideogram underline or right side line hardly ever supported */
  def ideogram(s: String) = apply(60)(s)
  
  /** ideogram double underline or double line on the right side  hardly ever supported */
  def ideogram(s: String) = apply(61)(s)
  
  /** ideogram overline or left side line hardly ever supported */
  def ideogram(s: String) = apply(62)(s)
  
  /** ideogram double overline or double line on the left side  hardly ever supported */
  def ideogram(s: String) = apply(63)(s)
  
  /** ideogram stress marking hardly ever supported */
  def ideogram(s: String) = apply(64)(s)
  
  /** ideogram attributes off hardly ever supported, reset the effects of all of 60–64 */
  def ideogram(s: String) = apply(65)(s)
  */
  
  /*
  /** Set foreground text color, high intensity aixterm (not in standard) */
  def set(s: String) = apply(90–97)(s)
  
  /** Set background color, high intensity  aixterm (not in standard) */
  def set(s: String) = apply(100–107)(s)
  */


  private def code(i: Int) = s"[${i}m"
  
  /*
  def ansi = apply _
  def apply(
    color: Color,
    subject = Foreground,


  )*/
}
