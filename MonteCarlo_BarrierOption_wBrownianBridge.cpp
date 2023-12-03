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
#include <iomanip>
#include <random>
#include <cmath>
#include <ctime>
#include <cstdlib>
#include <chrono>

using namespace std;

float up_factor, uptick_prob, risk_free_rate, strike_price, barrier_price;
float initial_stock_price, expiration_time, volatility, R;
int no_of_divisions, no_of_sampling_instants, no_of_discrete_barriers;



double max(double a, double b)
{
	return (b < a) ? a : b;
}

unsigned seed = (unsigned)chrono::system_clock::now().time_since_epoch().count();
default_random_engine generator;

double get_uniform()
{
	// http://www.cplusplus.com/reference/random/exponential_distribution/
	uniform_real_distribution<double> distribution(0.0, 1.0);
	double number = distribution(generator);
	return (number);
}

double N(const double z) {
	if (z > 6.0) { return 1.0; }; // this guards against overflow 
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

struct BrownianBridge_Prices
{
	double discrete_put_option_price = 0.0;
	double discrete_call_option_price = 0.0;
	double brownianbridge_call = 0.0;
	double brownianbridge_put = 0.0;
};

struct Adjust
{
	double call_price_via_simulation = 0.0;
	double adjustment_call_price = 0.0;
	double put_price_via_simulation = 0.0;
	double adjustment_put_price = 0.0;
};

Adjust european_continuous_down_and_out_option()
{
	Adjust adj;
	double intervalwidth, R2, SD;
	vector<double> stock(5, 0);

	intervalwidth = expiration_time / (double)no_of_divisions;
	R2 = (risk_free_rate - 0.5 * pow(volatility, 2)) * intervalwidth;
	SD = volatility * sqrt(intervalwidth);

	double sum_call = 0.0;
	double call_sim_temp = 0.0;
	double sum_put = 0.0;
	double put_sim_temp = 0.0;

	double adjustment_sum_for_call = 0.0;
	double adjustment_sum_for_put = 0.0;
	double probability_of_hitting;

	int sampler = 0;

	for (int i = 0; i < no_of_sampling_instants; i++) 
	{
		stock[1] = initial_stock_price;
		stock[2] = initial_stock_price;
		stock[3] = initial_stock_price;
		stock[4] = initial_stock_price;

		double divide_by_call_temp = 0.0;
		double divide_by_put_temp = 0.0;

		for (int j = 0; j < no_of_divisions; j++) 
		{
			double x = get_uniform();
			double y = get_uniform();
			double a = sqrt(-2.0 * log(x)) * cos(6.283185307999998 * y);
			double b = sqrt(-2.0 * log(x)) * sin(6.283185307999998 * y);
			stock[1] = stock[1] * exp(R2 + SD * a);
			stock[2] = stock[2] * exp(R2 - SD * a);
			stock[3] = stock[3] * exp(R2 + SD * b);
			stock[4] = stock[4] * exp(R2 - SD * b);
			for (int k = 1; k < 5; k++) 
			{
				if (stock[k] <= barrier_price) 
				{
					stock[k] = 0;
				}
			}
		}
		for (int k = 1; k < 5; k++) 
		{
			if (stock[k] > 0) 
			{
				divide_by_call_temp = divide_by_call_temp + max(0, stock[k] - strike_price);
				divide_by_put_temp = divide_by_put_temp + max(0, strike_price - stock[k]);
				probability_of_hitting = exp((-2) * log(initial_stock_price / barrier_price) * log(stock[k] / barrier_price) / (pow(volatility, 2.0) * expiration_time));
				adjustment_sum_for_call = adjustment_sum_for_call + max(0, stock[k] - strike_price) * (1 - probability_of_hitting);
				adjustment_sum_for_put = adjustment_sum_for_put + max(0, strike_price - stock[k]) * (1 - probability_of_hitting);
				sampler++;
			}
		}
		divide_by_call_temp = divide_by_call_temp / 4;
		sum_call = sum_call + divide_by_call_temp;
		divide_by_put_temp = divide_by_put_temp / 4;
		sum_put = sum_put + divide_by_put_temp;
	}
	call_sim_temp = sum_call / no_of_sampling_instants;
	put_sim_temp = sum_put / no_of_sampling_instants;
	adj.call_price_via_simulation = call_sim_temp / exp(risk_free_rate * expiration_time);
	adj.adjustment_call_price = adjustment_sum_for_call / (sampler * exp(risk_free_rate * expiration_time));
	adj.put_price_via_simulation = put_sim_temp / exp(risk_free_rate * expiration_time);
	adj.adjustment_put_price = adjustment_sum_for_put / (sampler * exp(risk_free_rate * expiration_time));

	return(adj);
}

BrownianBridge_Prices european_discrete_down_and_out_option()
{
	BrownianBridge_Prices bbp;
	bbp.brownianbridge_call = 0.0;
	bbp.brownianbridge_put = 0.0;
	int count = 0;
	bbp.discrete_put_option_price = 0.0;
	bbp.discrete_call_option_price = 0.0;
	double bridge_call = 0.0;
	double bridge_put = 0.0;
	double probability_of_brownian_bridge = 1;
	double SD2 = volatility * sqrt(expiration_time);
	float R2 = (risk_free_rate - 0.5 * pow(volatility, 2)) * expiration_time;

	for (int i = 0; i < no_of_sampling_instants; i++)
	{
		vector <double> current_stock_price(5, 0);

		float x = get_uniform();
		float y = get_uniform();
		float a = sqrt(-2.0 * log(x)) * cos(6.283185307999998 * y);
		float b = sqrt(-2.0 * log(x)) * sin(6.283185307999998 * y);

		current_stock_price[1] = initial_stock_price;
		current_stock_price[2] = initial_stock_price;
		current_stock_price[3] = initial_stock_price;
		current_stock_price[4] = initial_stock_price;

		current_stock_price[1] = current_stock_price[1] * exp(R2 + SD2 * a);
		current_stock_price[2] = current_stock_price[2] * exp(R2 - SD2 * a);
		current_stock_price[3] = current_stock_price[3] * exp(R2 + SD2 * b);
		current_stock_price[4] = current_stock_price[4] * exp(R2 - SD2 * b);

		double intervalrange = expiration_time;
		double intervalsize = no_of_discrete_barriers;
		double intervalwidth = expiration_time / (double)no_of_discrete_barriers;

		for (double intervalEnd = intervalsize; intervalEnd <= intervalrange; intervalEnd += intervalsize)
		{
			if (current_stock_price[1] < barrier_price)
			{
				current_stock_price[1] = 0.0;
			}
			if (current_stock_price[2] < barrier_price)
			{
				current_stock_price[2] = 0.0;
			}
			if (current_stock_price[3] < barrier_price)
			{
				current_stock_price[3] = 0.0;
			}
			if (current_stock_price[4] < barrier_price)
			{
				current_stock_price[4] = 0.0;
			}
		}
		for (int j = 1; j < 5; j++)
		{
			if (current_stock_price[j] > 0)
			{
				for (int k = 1; k < no_of_discrete_barriers; k++)
				{
					double mean_at_sampling_instance;
					double variance_at_sampling_instance;
					mean_at_sampling_instance = initial_stock_price + (intervalwidth * k / expiration_time) * (current_stock_price[j] - initial_stock_price);
					variance_at_sampling_instance = (intervalwidth * k * (expiration_time - intervalwidth * k)) / expiration_time;
					probability_of_brownian_bridge = probability_of_brownian_bridge * (1 - N((barrier_price - mean_at_sampling_instance) / sqrt(variance_at_sampling_instance)));
					bridge_call = bridge_call + max(0.0, current_stock_price[j] - strike_price) * (1 - probability_of_brownian_bridge);
					bridge_put = bridge_put + max(0.0, strike_price - current_stock_price[j]) * (1 - probability_of_brownian_bridge);
					count++;
					probability_of_brownian_bridge = exp(-2 * log(initial_stock_price / barrier_price) * log(current_stock_price[j] / barrier_price) / pow(volatility, 2.0) * expiration_time);
				}
			}
		}

		bbp.discrete_call_option_price += (max(0.0, current_stock_price[1] - strike_price) +
			max(0.0, current_stock_price[2] - strike_price) +
			max(0.0, current_stock_price[3] - strike_price) +
			max(0.0, current_stock_price[4] - strike_price)) / 4.0;
		bbp.discrete_put_option_price += (max(0.0, strike_price - current_stock_price[1]) +
			max(0.0, strike_price - current_stock_price[2]) +
			max(0.0, strike_price - current_stock_price[3]) +
			max(0.0, strike_price - current_stock_price[4])) / 4.0;

	}

	bbp.discrete_call_option_price = exp(-risk_free_rate * expiration_time) * (bbp.discrete_call_option_price / ((float)no_of_sampling_instants));
	bbp.discrete_put_option_price = exp(-risk_free_rate * expiration_time) * (bbp.discrete_put_option_price / ((float)no_of_sampling_instants));
	bbp.brownianbridge_call = bridge_call / (count * exp(risk_free_rate * expiration_time));
	bbp.brownianbridge_put = bridge_put / (count * exp(risk_free_rate * expiration_time));

	return bbp;
}



double option_price_call_BlackScholes(const double S, const double K, const double r, const double sigma, const double time)
{
	double time_sqrt = sqrt(time);
	double d1 = (log(S / K) + r * time) / (sigma * time_sqrt) + 0.5 * sigma * time_sqrt;
	double d2 = d1 - (sigma * time_sqrt);
	return S * N(d1) - K * exp(-r * time) * N(d2);
}

//vanilla option price Black-Scholes

double option_price_put_BlackScholes(const double S, const double K, const double r, const double sigma, const double time)
{
	double time_sqrt = sqrt(time);
	double d1 = (log(S / K) + r * time) / (sigma * time_sqrt) + 0.5 * sigma * time_sqrt;
	double d2 = d1 - (sigma * time_sqrt);
	return K * exp(-r * time) * N(-d2) - S * N(-d1);
}

double closed_form_down_and_out_european_call_option()
{
	double K = (2 * risk_free_rate) / (volatility * volatility);
	double A = option_price_call_BlackScholes(initial_stock_price, strike_price, risk_free_rate, volatility, expiration_time);
	double B = (barrier_price * barrier_price) / initial_stock_price;
	double C = pow(initial_stock_price / barrier_price, -(K - 1));
	double D = option_price_call_BlackScholes(B, strike_price, risk_free_rate, volatility, expiration_time);
	return (A - D * C);
}

double closed_form_down_and_in_european_put_option()
{
	double S = initial_stock_price;
	double r = risk_free_rate;
	double T = expiration_time;
	double sigma = volatility;
	double H = barrier_price;
	double X = strike_price;

	double lambda = (r + ((sigma * sigma) / 2)) / (sigma * sigma);
	double temp = 2 * lambda - 2.0;
	double x1 = (log(S / H) / (sigma * sqrt(T))) + (lambda * sigma * sqrt(T));
	double y = (log(H * H / (S * X)) / (sigma * sqrt(T))) + (lambda * sigma * sqrt(T));
	double y1 = (log(H / S) / (sigma * sqrt(T))) + (lambda * sigma * sqrt(T));
	return (-S * N(-x1) + X * exp(-r * T) * N(-x1 + sigma * sqrt(T)) +
		S * pow(H / S, 2 * lambda) * (N(y) - N(y1)) -
		X * exp(-r * T) * pow(H / S, temp) * (N(y - sigma * sqrt(T)) - N(y1 - sigma * sqrt(T))));
}

double closed_form_down_and_out_european_put_option()
{
	double vanilla_put = option_price_put_BlackScholes(initial_stock_price, strike_price,
		risk_free_rate, volatility, expiration_time);
	double put_down_in = closed_form_down_and_in_european_put_option();
	return (vanilla_put - put_down_in);
}

int main(int argc, char** argv)
{
	if (argc < 9)
	{
		cout << "Error, not enough inputs." << endl;
		return 0;
	}

	expiration_time = stod(argv[1]);
	risk_free_rate = stod(argv[2]);
	volatility = stod(argv[3]);
	initial_stock_price = stod(argv[4]);
	strike_price = stod(argv[5]);
	no_of_sampling_instants = stod(argv[6]);
	no_of_divisions = stoi(argv[7]);
	barrier_price = stod(argv[8]);
	no_of_discrete_barriers = stoi(argv[9]);

	if (barrier_price > initial_stock_price)
	{
		cout << "The value you entered is not a valid barrier for down and in/out." << endl;
		return false;
	}

	up_factor = exp(volatility * sqrt(expiration_time / ((float)no_of_divisions)));
	R = exp(risk_free_rate * expiration_time / ((float)no_of_divisions));
	uptick_prob = (R - (1 / up_factor)) / (up_factor - (1 / up_factor));
	double SD = volatility * sqrt(expiration_time);
	float R2 = (risk_free_rate - 0.5 * pow(volatility, 2)) * expiration_time;

	clock_t time_before_discrete_option, time_after_discrete_option;

	time_before_discrete_option = clock();
	european_discrete_down_and_out_option();
	time_after_discrete_option = clock();

	float diff_discrete_barrier_option = ((float)time_after_discrete_option - (float)time_before_discrete_option);

	float comp_time_discrete_barrier_option = diff_discrete_barrier_option / CLOCKS_PER_SEC;

	Adjust result2 = european_continuous_down_and_out_option();
	BrownianBridge_Prices result = european_discrete_down_and_out_option();
	

	cout << "Barrier Option Pricing" << endl;
	cout << "-----------------------------------------" << endl;
	cout << "-----------------------------------------" << endl;
	cout << "Expiration Time: " << expiration_time << endl;
	cout << "No of Discrete Divisions: " << no_of_divisions << endl;
	cout << "Risk Free Rate: " << risk_free_rate << endl;
	cout << "Volatility Spread (percentage): " << volatility * 100 << endl;
	cout << "Initial (Spot) Stock Price: " << initial_stock_price << endl;
	cout << "Strike Price: " << strike_price << endl;
	cout << "Barrier Price: " << barrier_price << endl;
	cout << "Number of Discrete Monte Carlo Sampling Instants: " << no_of_sampling_instants << endl;
	cout << "Number of Discrete Barriers: " << no_of_discrete_barriers << endl;
	cout << "-----------------------------------------" << endl;
	cout << "Up Factor: " << up_factor << endl;
	cout << "Uptick Probability: " << uptick_prob << endl;
	cout << "-----------------------------------------" << endl;
	cout << "Down and Out Discrete Barrier Option Price" << endl;
	cout << "-----------------------------------------" << endl;
	cout << "-----------------------------------------" << endl;
	cout << "The Average Call Price via Monte Carlo Simulation of Price Paths: " << result.discrete_call_option_price << endl;
	cout << "The Average Call Price via Brownian - Bridge Correction on Final Price: " << result.brownianbridge_call << endl;
	cout << "The Average Put Price via Monte Carlo Simulation of Price Paths: " << result.discrete_put_option_price << endl;
	cout << "The Average Put Price via Brownian - Bridge Correction on Final Price: " << result.brownianbridge_put << endl;
	cout << "The Computation Time of the Discrete Computation via Simulation: " << comp_time_discrete_barrier_option << endl;
	cout << "-----------------------------------------" << endl;
	cout << "Down and Out Continuous Discrete Barrier Option Price" << endl;
	cout << "-----------------------------------------" << endl;
	cout << "-----------------------------------------" << endl;
	cout << "The Average Call Price by Explicit Monte Carlo Simulation: " << result2.call_price_via_simulation << endl;
	cout << "The Call Price with the (1-p) - Adjustment Term: " << result2.adjustment_call_price << endl;
	cout << "Theoretical Price of a European Down and Out Call Option: " << closed_form_down_and_out_european_call_option() << endl;
	cout << "-----------------------------------------" << endl;
	cout << "The Average Put Price by Explicit Monte Carlo Simulation: " << result2.put_price_via_simulation << endl;
	cout << "The Put Price with the (1-p) - Adjustment Term: " << result2.adjustment_put_price << endl;
	cout << "Theoretical Price of a European Down and Out Put Option: " << closed_form_down_and_out_european_put_option() << endl;
	cout << "-----------------------------------------" << endl;

}