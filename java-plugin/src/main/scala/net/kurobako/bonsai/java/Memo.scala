package net.kurobako.bonsai.java

import javax.lang.model.element.Element

private object Memo {

  /** Whether the element holds, as cached, which the cache then holds if it has no answer yet. */
  def apply(cache: java.util.Map[Element, Boolean], element: Element)(compute: => Boolean): Boolean =
    if (cache.containsKey(element)) cache.get(element)
    else {
      val value = compute
      cache.put(element, value)
      value
    }
}
