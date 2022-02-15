public class 7 {
  class Point {
    private int x_, y_;
    Point(int x, int y) { x_ = x; y_ = y; }
    int getX() { return x_; }
    int getY() { return y_; }
    boolean equals( Point other) {
      System.out.println("1");
      return (this.getX() == other.getX())
      && (this.getY() == other.getY());
    }
  }
  class ColorPoint extends Point {
    private String c_ = "WHITE";
    ColorPoint(int x, int y) { super(x,y); c_="RED"; }
    String getColor() { return c_; }
    boolean equals( ColorPoint other) {
      System.out.println("1");
      return super.equals(other) && (this.getColor() == other.getColor());
    }
  }

  public static void main(String args[]) {
    Point pt1, pt2;
    ColorPoint cpt,cpt2;

    pt1 = new Point(3,5);
    cpt = new ColorPoint(3,5);
    cpt2 = new ColorPoint(3,5);
    pt2 = cpt; 

    boolean b =  pt2.equals(cpt2);
  }
}
