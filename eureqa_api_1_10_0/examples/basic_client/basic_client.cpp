#include <iostream>
#include <time.h>
#include <fstream>
#include <cassert>

using namespace std;


#include <eureqa/eureqa.h>

#ifdef WIN32
inline void sleep(int sec) { Sleep(sec*1000); }
#endif

void pause_exit(int code)
{
    std::cout << std::endl;
    #ifdef WIN32
    system("pause");
	#endif
    exit(code);
}

int main(int argc, char *argv[])
{
    std::cout << "The basic_client attempts to connect to a Eureqa Server ";
    std::cout << "running on the local machine and perform a search. It ";
    std::cout << "is an extension of the minimal_client, with informational ";
    std::cout << "messages and proper error checking. It is meant to be ";
    std::cout << "used as a source code example." << std::endl;
    std::cout << std::endl;
    
    // initialize a data set
    std::cout << std::endl;
    std::cout << "> Importing data set from a text file" << std::endl;

    eureqa::data_set data; // holds the data
    std::string error_msg; // error information during import

    // import data from a text file
    if (!data.import_ascii(argv[1], error_msg))
    {
        std::cout << error_msg << std::endl;
        std::cout << "Unable to import this file" << std::endl;
        pause_exit(-1);
    }
    
    // print information about the data set
    std::cout << "Data set imported successfully" << std::endl;
    std::cout << data.summary() << std::endl;
    
    // initialize search options
    eureqa::search_options options("USOL = f(SPY)");

//eureqa::search_options options(argv[3]); // holds the search options
	
    //options.search_relationship_ = argv[3];
    //options.normalize_fitness_by_ = 10.0;
    //options.fitness_metric_ = eureqa::fitness_types::absolute_error;
    //options.solution_population_size_ = 100;
    //options.predictor_population_size_ = 10;
    //options.trainer_population_size_ = 10;
    //options.solution_crossover_probability_ = 0.5;
    //options.solution_mutation_probability_ = 0.01;
    //options.predictor_crossover_probability_ = 0.5;
    //options.predictor_mutation_probability_ = 0.2;
    //options.implicit_derivative_dependencies_ = "";

	// search for a formula f that satisfies:
  
	std::cout << std::endl;
    std::cout << "> Setting the search options" << std::endl;
    std::cout << options.summary() << std::endl;
		
	options.set_default_building_blocks();

	
    // connect to a eureqa server
    eureqa::connection conn;
    std::cout << std::endl;
    std::cout << "> Connecting to a eureqa server at 213.136.18.153" << std::endl;

    if (!conn.connect("213.136.18.153"))
    {
        std::cout << "Unable to connect to server" << std::endl; 
		std::cout << "Try running the eureqa_server binary provided with ";
		std::cout << "the Eureqa API (\"server\" sub-directory) first." << std::endl;
        pause_exit(-1);
    }
    else if (!conn.last_result()) 
    { 
        std::cout << "Connection made successfully, but ";
        std::cout << "the server sent back an error message:" << std::endl;
        std::cout << conn.last_result() << std::endl;
        pause_exit(-1); 
    }
    else
    {
        std::cout << "Connected to server successfully, and ";
        std::cout << "the server sent back a success message:" << std::endl;
        std::cout << conn.last_result() << std::endl;
    }
    
    // query the server's information
    eureqa::server_info serv;
    std::cout << std::endl;
    std::cout << "> Querying the server systems information" << std::endl;
    
    if (!conn.query_server_info(serv))
    {
        std::cout << "Unable to recieve the server information" << std::endl;
        pause_exit(-1);
    }
    else
    {
        std::cout << "Recieved server information successfully:" << std::endl;
        std::cout << serv.summary() << std::endl;
    }
    
    // send data set
    std::cout << std::endl;
    std::cout << "> Sending the data set to the server" << std::endl;
    
    if (!conn.send_data_set(data))
    {
        std::cout << "Unable to transfer the data set" << std::endl;
        pause_exit(-1);
    }
    else if (!conn.last_result())
    {
        std::cout << "Data set transferred successfully, but ";
        std::cout << "the server sent back an error message:" << std::endl;
        std::cout << conn.last_result() << std::endl;
        pause_exit(-1);
    }
    else
    {
        std::cout << "Data set transferred successfully, and ";
        std::cout << "the server sent back a success message:" << std::endl;
        std::cout << conn.last_result() << std::endl;
    }
    
    // send options
    std::cout << std::endl;
    std::cout <<  "> Sending search options to the server" << std::endl;
    
    if (!conn.send_options(options))
    {
        std::cout << "Unable to transfer the search options" << std::endl;
        pause_exit(-1);
    }
    else if (!conn.last_result())
    {
        std::cout << "Search options transferred successfully, but ";
        std::cout << "the server sent back an error message:" << std::endl;
        std::cout << conn.last_result() << std::endl;
        pause_exit(-1);
    }
    else
    {
        std::cout << "Search options transferred successfully, and ";
        std::cout << "the server sent back a success message:" << std::endl;
        std::cout << conn.last_result() << std::endl;
    }
    
    // start searching
    std::cout << std::endl;
    std::cout << "> Telling server to start searching" << std::endl;
    
    if (!conn.start_search())
    {
        std::cout << "Unable to send the start command" << std::endl;
        pause_exit(-1);
    }
    else if (!conn.last_result())
    {
        std::cout << "Start command sent successfully, but ";
        std::cout << "the server sent back an error message:" << std::endl;
        std::cout << conn.last_result() << std::endl;
        pause_exit(-1);
    }
    else
    {
        std::cout << "Start command sent successfully, and ";
        std::cout << "the server sent back a success message:" << std::endl;
        std::cout << conn.last_result() << std::endl;
    }
    
    // monitor the search
    std::cout << std::endl;
    std::cout << "> Monitoring the search progress" << std::endl;
    
    eureqa::search_progress progress; // recieves the progress and new solutions
    eureqa::solution_frontier best_solutions; // filters out the best solutions
    
    // continue searching (until user hits ctrl-c)
    while (conn.query_progress(progress))
    {
        // print the progress (e.g. number of generations)
        std::cout << "> " << progress.summary() << std::endl;
        std::cout << std::endl;
        
        // the eureqa server sends a stream of new solutions in the progress
        // here we filter out and store only the best solutions
        if (best_solutions.add(progress.solution_))
        {
            std::cout << "New solution found:" << std::endl;
            std::cout << progress.solution_ << std::endl;
        }
        
        // print a display of the best solutions
        std::cout << std::endl;
        std::cout << best_solutions.to_string() << std::endl;
        std::cout << std::endl;
		ofstream myfile (argv[2]);
		if (myfile.is_open())
        {
        myfile << best_solutions.to_string() << std::endl;
        myfile.close();
        }

        // update every second
        sleep(1);
    }
    
    pause_exit(0);
    return 0;
}
