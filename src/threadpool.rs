use std::{
    num::NonZeroUsize,
    sync::{mpsc, Arc, Mutex},
    thread,
};

fn n_cpus() -> NonZeroUsize {
    thread::available_parallelism().expect("amount of available parallelism is unknown")
}

pub struct ThreadPool<F, T> {
    workers: Vec<Worker>,
    job_sender: mpsc::Sender<F>,
    output_receiver: mpsc::Receiver<T>,
}

impl<F, T> ThreadPool<F, T>
where
    F: FnOnce() -> T,
    F: Send + 'static,
    T: Send + 'static,
{
    pub fn new(n_workers: Option<NonZeroUsize>) -> Self {
        let n_workers = n_workers.unwrap_or_else(n_cpus).get();
        let mut workers = Vec::with_capacity(n_workers);

        let (job_sender, job_receiver) = mpsc::channel();
        let job_receiver = Arc::new(Mutex::new(job_receiver));

        let output_receiver = {
            let (output_sender, output_receiver) = mpsc::channel();

            for id in 0..n_workers {
                workers.push(Worker::new(
                    id,
                    Arc::clone(&job_receiver),
                    output_sender.clone(),
                ));
            }

            output_receiver
        };

        ThreadPool {
            workers,
            job_sender,
            output_receiver,
        }
    }

    pub fn execute(&self, f: F) {
        self.job_sender
            // .as_ref()
            // .expect("no job sender available")
            .send(f)
            .expect("no job receivers available to run jobs");
    }

    pub fn outputs(self) -> Vec<T> {
        // self
        // .output_receiver

        drop(self.job_sender);
        for worker in self.workers {
            worker.thread.join().unwrap();
        }

        self.output_receiver
            // .as_ref()
            // .expect("no output receiver available")
            .into_iter()
            .collect()

        // println!("\n\n\nhere\n\n\n");

        // o
    }
}

struct Worker {
    id: usize,
    thread: thread::JoinHandle<()>,
}

impl Worker {
    fn new<F, T>(
        id: usize,
        job_receiver: Arc<Mutex<mpsc::Receiver<F>>>,
        output_sender: mpsc::Sender<T>,
    ) -> Self
    where
        F: FnOnce() -> T,
        F: Send + 'static,
        T: Send + 'static,
    {
        let thread = thread::spawn(move || loop {
            let job = job_receiver
                .lock()
                .expect("cannot acquire mutex lock")
                .recv();

            match job {
                Ok(job) => {
                    let output = job();
                    match output_sender.send(output) {
                        Ok(_) => {}
                        Err(_) => {
                            break;
                        }
                    }
                }
                Err(_) => {
                    break;
                }
            };
        });

        Worker { id, thread }
    }
}
