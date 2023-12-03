/*
	CREDITS : received help from another student in class who will remain anonymous
	CREDITS v2: N function code was taken from a paper that is well cited
	CREDITS v3: some content/lecture notes were taken from Prof. Ramavarapu Sreenivas UIUC - (c) 
 */





#include <iostream>
#include <vector>
#include <math.h>
#include <string>
#include <algorithm>
#include <ctime>
#include <random>
#include <iomanip>
#include <unordered_map>
#include <cmath>

using namespace std;

double up_factor, uptick_prob, downtick_prob, risk_free_rate, strike_price;
double initial_price, expiration, volatility, R;
int stages; 
double no_tick;

vector<vector<double>> European_Call_Cache;
vector<vector<double>> European_Put_Cache;
vector<vector<double>> American_Call_Cache;
vector<vector<double>> American_Put_Cache;

double max(double a, double b)
{
	return (b < a)? a : b;
}

double N(const double& z)
{
	if (z > 6.0) { return 1.0; };
	if (z < -6.0) { return 0.0; };
	double b1 = 0.31938153;
	double b2 = -0.356563782;
	double b3 = 1.781477937;
	double b4 = -1.821255978;
	double b5 = 1.330274429;
	double p = 0.2316419;
	double c2 = 0.3989423;
	double a = fabs(z);
	double t = 1.0 / (1.0 + a * p);
	double b = c2 * exp((-z) * (z / 2.0));
	double n = ((((b5 * t + b4) * t + b3) * t + b2) * t + b1) * t;
	n = 1.0 - b * n;
	if (z < 0.0) n = 1.0 - n;
	return n;
}

double SD(double e, double v)
{
	return (v * sqrt(e));
}

double Black_Scholes_Put(const double e, const double rfr, const double v, const double initial, const double strike)
{
	double d1 = (log(initial / strike) + (rfr * e)) / (SD(e, v)) + 0.5 * SD(e, v);
	double d2 = d1 - (SD(e, v));
	return (strike * exp(-rfr * e) * N(-d2)) - (initial * N(-d1));
}

double Black_Scholes_Call(const double e, const double rfr, const double v, const double initial, const double strike)
{
	double d1 = (log(initial / strike) + (rfr * e)) / (SD(e, v)) + 0.5 * SD(e, v);
	double d2 = d1 - (SD(e, v));
	return (initial * N(d1)) - (strike * exp(-rfr * e) * N(d2));
}

double European_Put(int k, int i, double price)
{

	if(European_Put_Cache[k][i] != -1){
		return European_Put_Cache[k][i];
	}
	else
	{
		if (k == stages)
		{
			return max(0.0, strike_price - price);
		}
		
		double value_option_temp = ((uptick_prob * European_Put(k + 1, i + 1, price * up_factor)) + ((1 - uptick_prob - downtick_prob) * European_Put(k + 1, i, price)) 
			+ (downtick_prob * European_Put(k + 1, i - 1, price / up_factor))) / R; //come back to this -- returning incorrect trinomial value, not taking max


		European_Put_Cache[k][i] = value_option_temp;

		return value_option_temp;
	}
}


double European_Call(int k, int i, double price)
{

	if(European_Call_Cache[k][i] != -1){
		return European_Call_Cache[k][i];
	}

	else
	{
		if (k == stages)
		{
			return max(0.0, price - strike_price);
		}
		
		double value_option_temp = ((uptick_prob * European_Call(k + 1, i + 1, price * up_factor)) + ((1 - uptick_prob - downtick_prob) * European_Call(k + 1, i, price))
			+ (downtick_prob * European_Call(k + 1, i - 1, price / up_factor))) / R;

		European_Call_Cache[k][i] = value_option_temp;
		return value_option_temp;
	}
}

double American_Put(int k, int i, double current_stock_price)
{

	if( American_Put_Cache[k][i] != -1){
		return American_Put_Cache[k][i];
	}
	else
	{
		if (k == stages)
		{
			return max(0.0, (strike_price - current_stock_price));
		}
		double value_option_temp = max((strike_price - current_stock_price), ((uptick_prob*American_Put(k + 1, i + 1, current_stock_price * up_factor)) 
			+ (1 - uptick_prob - downtick_prob) * American_Put(k + 1, i, current_stock_price) + (downtick_prob * American_Put(k + 1, i - 1, current_stock_price / up_factor))) / R);

		American_Put_Cache[k][i] = value_option_temp;

		return value_option_temp;
	}
}

double American_Call(int k, int i, double current_stock_price)
{

	if(American_Call_Cache[k][i] != -1){
		return American_Call_Cache[k][i];
	}
	else
	{
		if (k == stages)
		{
			return max(0.0, (current_stock_price - strike_price));
		}
		double value_option_temp = max((current_stock_price - strike_price), ((uptick_prob * American_Call(k + 1, i + 1, current_stock_price * up_factor))
			+ (1 - uptick_prob - downtick_prob) * American_Call(k + 1, i, current_stock_price) + (downtick_prob * American_Call(k + 1, i - 1, current_stock_price / up_factor))) / R);

		American_Call_Cache[k][i] = value_option_temp;

		return value_option_temp;
	}
}

int main(int argc, char** argv)
{
	if (argc < 6)
	{
		cout << "Error, not enough values entered." << endl;
		return 69;
	}

	expiration = stod(argv[1]);
	stages = stoi(argv[2]);
	risk_free_rate = stod(argv[3]);
	volatility = stod(argv[4]);
	initial_price = stod(argv[5]);
	strike_price = stod(argv[6]);

	up_factor = exp(volatility * sqrt(2 * (expiration / ((double)stages))));
	R = exp(risk_free_rate * expiration / ((double)stages));
	uptick_prob = pow(((sqrt(R) - (1 / sqrt(up_factor))) / (sqrt(up_factor) - (1 / sqrt(up_factor)))), 2);
	downtick_prob = pow(((sqrt(up_factor) - sqrt(R)) / (sqrt(up_factor) - (1 / sqrt(up_factor)))), 2);

	European_Call_Cache.assign(stages+1,vector<double>(2 * stages + 1, -1));
	European_Put_Cache.assign(stages + 1 , vector<double> (2 * stages + 1, -1));
	American_Call_Cache.assign(stages + 1 , vector<double> (2 * stages + 1, -1));
	American_Put_Cache.assign(stages + 1 , vector<double> (2 * stages + 1, -1));

	clock_t time_before_European_Call_Trinomial, time_after_European_Call_Trinomial;
	clock_t time_before_European_Put_Trinomial, time_after_European_Put_Trinomial;
	clock_t time_before_American_Call_Trinomial, time_after_American_Call_Trinomial;
	clock_t time_before_American_Put_Trinomial, time_after_American_Put_Trinomial;

	time_before_European_Call_Trinomial = clock();
	double call_price = European_Call(0.0, stages, initial_price);
	time_after_European_Call_Trinomial = clock();

	time_before_European_Put_Trinomial = clock();
	double put_price = European_Put(0.0, stages, initial_price);
	time_after_European_Put_Trinomial = clock();

	time_before_American_Call_Trinomial = clock();
	double American_call_price = American_Call(0.0, stages, initial_price);
	time_after_American_Call_Trinomial = clock();

	time_before_American_Put_Trinomial = clock();
	double American_put_price = American_Put(0.0, stages, initial_price);
	time_after_American_Put_Trinomial = clock();

	double diff_EuropeanCallTrinomial;
	double diff_EuropeanPutTrinomial;
	double diff_AmericanCallTrinomial;
	double diff_AmericanPutTrinomial;

	double comp_time_EuropeanCallTrinomial;
	double comp_time_EuropeanPutTrinomial;
	double comp_time_AmericanCallTriomial;
	double comp_time_AmericanPutTrinomial;

	diff_EuropeanCallTrinomial = ((double)time_after_European_Call_Trinomial - (double)time_before_European_Call_Trinomial);
	diff_EuropeanPutTrinomial = ((double)time_after_European_Put_Trinomial - (double)time_before_European_Put_Trinomial);
	diff_AmericanCallTrinomial = ((double)time_after_American_Call_Trinomial - (double)time_before_American_Call_Trinomial);
	diff_AmericanPutTrinomial = ((double)time_after_American_Put_Trinomial - (double)time_before_American_Put_Trinomial);

	comp_time_EuropeanCallTrinomial = diff_EuropeanCallTrinomial / CLOCKS_PER_SEC;
	comp_time_EuropeanPutTrinomial = diff_EuropeanPutTrinomial / CLOCKS_PER_SEC;
	comp_time_AmericanCallTriomial = diff_AmericanCallTrinomial / CLOCKS_PER_SEC;
	comp_time_AmericanPutTrinomial = diff_AmericanPutTrinomial / CLOCKS_PER_SEC;

	cout << "Recursive Trinomial Option Pricing Using Memoization" << endl;
	cout << "Expiration Time (Years) = " << expiration << endl;
	cout << "Number of Divisions = " << stages << endl;
	cout << "Risk Free Interest Rate = " << risk_free_rate << endl;
	cout << "Volatility (%age of stock value) = " << volatility * 100 << endl;
	cout << "Initial Stock Price = " << initial_price << endl;
	cout << "Strike Price = " << strike_price << endl;
	cout << "--------------------------------------" << endl;
	cout << "Up Factor = " << up_factor << endl;
	cout << "Uptick Probability = " << uptick_prob << endl;
	cout << "Downtick Probability = " << downtick_prob << endl;
	cout << "Notick Probability = " << 1- uptick_prob - downtick_prob << endl;
	cout << "--------------------------------------" << endl;

	// double call_price = European_Call(0.0, 0.0, initial_price);
	cout << "The price of the European Call Option using the Black_Scholes Model is: " << Black_Scholes_Call(expiration, risk_free_rate, volatility, initial_price, strike_price) << endl;
	cout << "The price of the European Call Option using the Trinomial Model is: " << call_price << endl;
	cout << "The computation time of the European Call Option was: " << comp_time_EuropeanCallTrinomial << endl;
	cout << "--------------------------------------" << endl;
	// double put_price = European_Put(0.0, 0.0, initial_price);
	cout << "The price of the European Put Option using the Black-Scholes Model is: " << Black_Scholes_Put(expiration, risk_free_rate, volatility, initial_price, strike_price) << endl;
	cout << "The price of the European Put Option using the Trinomial Model is: " << put_price << endl;
	cout << "The computation time of the European Put Option was: " << comp_time_EuropeanPutTrinomial << endl;
	cout << "--------------------------------------" << endl;
	// double American_put_price = American_Put(0.0, 0.0, initial_price);
	cout << "The Trinomial Price of the American Put Option is: " << American_put_price << endl;
	cout << "The computation time of the American Put Option was: " << comp_time_AmericanPutTrinomial << endl;
	// double American_call_price = American_Call(0.0, 0.0, initial_price);
	cout << "--------------------------------------" << endl;
	cout << "The Trinomial Price of the American Call Option is: " << American_call_price << endl;
	cout << "The computation time of the American Put Option was: " << comp_time_EuropeanPutTrinomial << endl;

}