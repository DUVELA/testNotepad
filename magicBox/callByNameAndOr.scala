def and(x: Boolean, y: => Boolean) = if (x) y else false

def or(x: Boolean, y: => Boolean) = if (x) ture else y
/**
* def or(x:Boolean, y: => Boolean) = if (x) true else y에서도 
* y는 call-by-name으로 정의되어 있습니다. 
* 이렇게 하면 x 값이 true일 경우에는 true를 반환하고 y의 평가가 발생하지 않습니다. 
* 하지만 x 값이 false일 경우에는 y가 평가되어 결과를 결정합니다. 
* 이런 방식으로 or 함수의 동작이 최적화되며, y의 평가는 필요한 경우에만 수행되게 됩니다.
* chatGPT
*
