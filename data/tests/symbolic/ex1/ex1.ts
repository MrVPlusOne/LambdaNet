class Num{

  constructor(public v){}

  public plus(that: Num): Num {
    return new Num(this.v + that.v)
  }
}