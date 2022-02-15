#include<iostream>
#include<string>

using namespace std;

class Point{
public:
	Point( int x, int y){
		x_ = x;
		y_ = y;
	}
	virtual ~Point(){}
	int getX(){ return x_; }
	int getY(){ return y_; }
	bool equals( Point* other )
	{
		return (this->getX() == other->getX()) && (this->getY() == other->getY());
	}
private:
	int x_, y_;
};

class ColorPoint: public Point{
public:
	ColorPoint( int x, int y , string c = "WHITE" ):Point( x, y ){
		color_ = c;
	}
	string getColor(){ return color_; }
	void setColor( string c ){ color_ = c; }
	bool equals( Point *other ){
		cout<<"1"<<endl;
		ColorPoint * C_other = dynamic_cast<ColorPoint *>(other);
		if( C_other ){
			cout<<"CMP between ColorPoints"<<endl;
			return Point::equals(other) && this->getColor() == C_other->getColor();
		}
		return Point::equals(other);
	}
	bool equals( ColorPoint *other ){
		cout<<"2"<<endl;
		ColorPoint * C_other = dynamic_cast<ColorPoint *>(other);
		if( C_other ){
			cout<<"CMP between ColorPoints"<<endl;
			return Point::equals(other) && this->getColor() == C_other->getColor();
		}
		return Point::equals(other);
	}
private:
	string color_ = "WHITE";
};



int main(){
	Point *pt = new Point(3, 5);
	ColorPoint *pt2 = new ColorPoint(3, 5 , "RED");
	ColorPoint *pt3 = new ColorPoint(3, 5 , "BLACK");

	if( pt3-> equals(pt) )
		cout<<"same"<<endl;
	else
		cout<<"diff"<<endl;
}
