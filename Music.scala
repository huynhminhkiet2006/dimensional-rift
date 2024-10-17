import javax.sound.sampled.*

object MusicManager:

  private var clip: Option[Clip] = None

  def playMusicOnce(musicName: String) =
    stopMusic()

    try
      val audioInputStream = AudioSystem.getAudioInputStream(getClass.getResource(s"resources/soundtrack/$musicName.wav"))
      clip = Some(AudioSystem.getClip)
      clip.foreach:
        c =>
          c.open(audioInputStream)
          c.start()
          c.addLineListener(event =>
            if (event.getType == LineEvent.Type.STOP) then {
              c.close()
            }
          )

    catch
      case e: Exception =>
        println(s"Error playing music $musicName: ${e.getMessage}")

  def loopMusic(musicName: String) =
    stopMusic()

    try
      val audioInputStream = AudioSystem.getAudioInputStream(getClass.getResource(s"resources/soundtrack/$musicName.wav"))
      clip = Some(AudioSystem.getClip)
      clip.foreach( c =>
        c.open(audioInputStream)
        c.loop(Clip.LOOP_CONTINUOUSLY)
        c.start()
      )
    catch
      case e: Exception =>
        println(s"Error playing music $musicName: ${e.getMessage}")

  def stopMusic() =
    clip.foreach ( c =>
      c.close()
    )

    clip = None