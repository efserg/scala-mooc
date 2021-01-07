package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_caesar {

  private val BASE = 'A' - 1
  private val LETTERS = 26
  /**
   * В данном задании Вам предлагается реализовать функции,
   * реализующие кодирование/декодирование строки шифром Цезаря.
   * https://ru.wikipedia.org/wiki/Шифр_Цезаря
   * Алфавит - прописные латинские буквы от A до Z.
   * Сдвиг   - неотрицательное целое число.
   * Пример: при сдвиге 2 слово "SCALA" шифруется как "UECNC".
   */
  /**
   * @param word   входное слово, которое необходимо зашифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return зашифрованное слово
   */
  def encrypt(word: String, offset: Int): String =
    word
      .map(_ - BASE)
      .map(_ + offset % LETTERS)
      .map(_ % LETTERS + BASE)
      .map(_.toChar)
      .mkString

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String =
    cipher
      .map(_ - BASE + LETTERS)
      .map(_ - offset % LETTERS)
      .map(_ % LETTERS + BASE)
      .map(_.toChar)
      .mkString

}
